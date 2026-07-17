---
title: Analisi architettura frontend
---

# Analisi architettura frontend — RST Manager

Revisione dell'architettura Scala.js/Laminar del frontend, catalogata per impatto sulle prestazioni e sulla manutenibilità.

## Panoramica

L'architettura usa un pattern **pull-based** centralizzato (`AppBus`): tutte le pagine ricaricano i dati agganciandosi a un unico segnale globale, e ogni mutazione incrementa quel segnale. È funzionante e feature-complete, ma presenta inefficienze che crescono proporzionalmente alla complessità delle pagine.

---

## Problemi catalogati per impatto

### ALTO

#### 1. Global cascading refresh

Ogni mutazione chiama `AppBus.mutated()`, che incrementa `version` immediatamente e pianifica altri due incrementi a +1200 ms e +3200 ms. Tutte le pagine aperte si aggiornano a ogni tick.

Dopo una semplice modifica a un ordine vengono lanciate potenzialmente **15+ richieste HTTP** distribuite su 3,2 secondi (5 endpoint × 3 tick) — di cui la maggior parte è ridondante.

**Soluzione**: invalidazione granulare per dominio (stream separati in `AppBus`).

#### 2. Nessun caching tra reload

`loadable()` è un reload puro: ogni tick scarica tutto da zero. Non c'è stale-while-revalidate né cache in memoria. Navigare tra pagine forza ri-fetch di dati già scaricati.

**Soluzione**: cache a livello `Signal` con TTL o basata su invalidazione.

#### 3. Nessuna deduplicazione delle richieste

`OrdersPage` e `PlanningPage` caricano entrambe `listOrders()`, `listEmployees()`, `listTasks()`, `listManufacturingCatalog()` in parallelo (pagine distinte montate allo stesso tempo). Le richieste si raddoppiano senza coalescing.

**Soluzione**: in-flight deduplication nell'`ApiClient`: richieste GET identiche già in volo restituiscono la stessa `Future`. ✅ **Implementato.**

#### 4. Frammentazione estrema dello stato in `OrdersPage`

`OrdersPage.scala` (1202 righe) ha 53+ `Var` top-level più strutture annidate `Var[List[MfgDraft]]` dove ogni `MfgDraft` ha `Var[List[TaskDraft]]` con altri `Var` interni.

```
mfgs: Var[List[MfgDraft]]
  └─ MfgDraft.tasks: Var[List[TaskDraft]]
       └─ TaskDraft.taskId: Var[UUID]
       └─ TaskDraft.hours: Var[String]
       └─ TaskDraft.dependsOn: Var[List[UUID]]
```

Impossibile resettare il form atomicamente, nessuna type-safety, segnali compositi difficili da derivare.

**Soluzione**: consolidare in case class immutabili con un singolo `Var[CreateOrderForm]`.

---

### MEDIO

#### 5. Polling di `PlanningPage` su AppBus globale

`PlanningPage` esegue `setInterval(15s)(AppBus.refresh())` mentre è montata. Questo chiama `AppBus.refresh()` che incrementa il tick globale, causando il reload di tutti i dati su tutte le pagine aperte — non solo della pianificazione.

Si sovrappone inoltre ai delayed tick di `AppBus.mutated()` (immediato + 1,2s + 3,2s): dopo una mutazione la planning viene ricaricata 4+ volte in 3 secondi.

**Soluzione**: isolare il poll su un tick locale di pagina; solo `planningData` si aggiorna sul poll, gli altri dati restano agganciati ad `AppBus.ticks`. ✅ **Implementato.**

#### 6. Pattern snapshot-Var manuale

In `ManufacturingsPage` e `OrdersPage` i dati della lista vengono copiati in un `Var` secondario via observer per usi imperativi (generazione codici, lookup sincrono). Rompe la single-source-of-truth e può causare desync se l'observer non si aggiorna in tempo.

```scala
// anti-pattern
val manufacturingCatalogs = Var(List.empty[ManufacturingCatalogResponse])
manufacturingCatalogData --> {
  case Some(Right(list)) => manufacturingCatalogs.set(list)
  case _ => ()
}
```

In `ManufacturingsPage` il pattern è aggravato dall'update manuale anche in `submit()`, creando una doppia sorgente di scrittura. Il `Var` è necessario per uso imperativo (`.now()` su segnali derivati non è disponibile), ma la doppia scrittura in `submit()` è ridondante: è stata rimossa, lasciando l'aggiornamento solo all'observer. ✅ **Implementato.**

#### 7. Frammentazione stato form nelle altre pagine

| Pagina | Var form | Note |
|--------|----------|------|
| `EmployeesPage` | 16 | create (8) + override editor (8) |
| `CustomersPage` | 10 | flat |
| `ManufacturingsPage` | 7 + nested | TaskRow annidata |

**Soluzione uniforme**: `Var[FormState]` con case class + reset atomico.
✅ **Implementato.**

#### 8. Nessuna validazione live

Validazione solo al submit: l'utente scopre gli errori dopo aver cliccato. Nessun segnale `formValid: Signal[Boolean]` derivato dai campi.

**Soluzione**: derivare `val formErrors: Signal[List[String]]` dai segnali di input; disabilitare il bottone submit reattivamente.

#### 9. Change detection manuale nei modal

Sia `OrdersPage` che `PlanningPage` tracciano manualmente i valori originali per rilevare modifiche:

```scala
case class TaskEditRow(
  originalExpected: Int,  // snapshot
  originalCompleted: Int, // snapshot
  expected: Var[String],  // mutable
  completed: Var[String], // mutable
)
```

**Soluzione**: un generico `DirtyTracker[A](initial: A, current: Var[A])` con `isDirty: Signal[Boolean]`. ✅ **Implementato.**

---

### BASSO / Qualità

#### 10. Nessun aggiornamento ottimistico

Tutte le mutazioni aspettano la risposta server. Per azioni come "segna task completato" o "cambia priorità" un ottimistic update renderebbe l'UI percettibilmente più veloce.

#### 11. Gestione errori dispersa

Ogni pagina ha il proprio `Var[Option[ApiError]]` gestito indipendentemente. Non c'è logging centralizzato né error boundary.

**Soluzione**: `ErrorCenter` centralizzato con logging console, banner globale dismissibile nell'app shell, handler runtime per errori frontend non gestiti e helper `showError(...)` per mantenere i banner locali senza duplicare il percorso di segnalazione. Anche i `loadable` segnalano gli errori di caricamento al centro errori. ✅ **Implementato.**

#### 12. Nessuna paginazione

Liste caricate interamente. Con 200+ ordini il rendering DOM completo diventa lento.

---

## Matrice priorità

| # | Problema | Effort | Impatto | Stato |
|---|----------|--------|---------|-------|
| 3 | No request deduplication | Basso | Alto | ✅ Risolto |
| 5 | Polling PlanningPage su AppBus globale | Basso | Medio | ✅ Risolto |
| 6 | Double-write snapshot in ManufacturingsPage | Basso | Medio | ✅ Risolto |
| 1 | Global cascading refresh | Alto | Alto | ✅ Risolto |
| 2 | No caching tra reload | Medio | Alto | ✅ Risolto |
| 4 | 53+ Var in OrdersPage | Alto | Alto | ✅ Risolto |
| 7 | Form state frammentato (altre pagine) | Alto | Medio | ✅ Risolto |
| 8 | No validazione live | Medio | Medio | ✅ Risolto |
| 9 | Change detection manuale | Basso | Medio | ✅ Risolto |
| 10 | No ottimistic updates | Alto | Basso | — |
| 11 | Gestione errori dispersa | Medio | Basso | ✅ Risolto |
| 12 | No paginazione | Alto | Basso (ora) | — |
