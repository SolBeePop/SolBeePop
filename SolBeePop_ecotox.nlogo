__includes ["BeeGUTS.nls"]

extensions [ csv ]

globals [
  list.input.files ; list of file names from file 'List.input.floral'
  list.DD.thresh   ; list of thresholds for density dependence (DD.thresh) for each simulated year (from file 'List.input.floral')
  list.DD.max.cells    ; list of maximum cell space for density dependence (DD.max.cells) for each simulated year (from file 'List.input.floral')
  doy              ; day of year
  year             ; simulated year
  sim.yrs          ; total years simulated (defined by number of columns in the input.Q.floral file)
  f.list           ; list of daily values defining the floral resource site (from input file)
  prop.forag.day   ; proportion of each day with foraging conditions (weather)
  Q.crop           ; daily quality of crop (potentially treated) floral resource site
  Q.nat            ; daily quality of off-crop (semi-natural, not treated) floral resource site
  crop.forag       ; daily proportion of foraging on crop (reaminder of foraging occurs on non-crop)
  DD.thresh        ; year-specific threshold for onset of density-dependent cell production rate
  DD.max.cells     ; year-specific maximum of total cells available in density-dependent simulations
  bees.emerged.yr  ; emerged bees (females and males) in a simulated year
  f.emerged.yr     ; emerged females in a simulated year
  m.emerged.yr     ; emerged males in a simulated year
  f.postemergent.today ; daily number of post-emergent females
  bees.nesting     ; total number of female bees that reached nesting stage in a simulated year
  bees.nesting.today   ; daily number of female bees in nesting stage
  sum.cells.today  ; daily total number of cells built
  sum.f.cells.today    ; daily total number of cells with female offspring
  sum.m.cells.today    ; daily total number of cells with male offspring
  sum.cells        ; total number of cells built; reset to 0 at the beginning of the year
  sum.f.cells      ; total number of cells with female offspring; reset to 0 at the beginning of the year
  sum.m.cells      ; total number of cells with male offspring; reset to 0 at the beginning of the year
  mean.cells.today ; daily mean number of cells built per nesting female
  mean.f.cells.today   ; daily mean number of cells with female offspring per nesting female
  mean.m.cells.today   ; daily mean number of cells with male offspring per nesting female
  mean.cells       ; mean number of cells built per nesting female to date; reset to 0 at the beginning of the year
  mean.f.cells     ; mean number of cells with female offspring per nesting female to date; reset to 0 at the beginning of the year
  mean.m.cells     ; mean number of cells with male offspring per nesting female to date; reset to 0 at the beginning of the year
  prov.crop.today  ; daily provision collected from crop
  prov.nat.today   ; daily provision collected from non-crop, note that the two added result in value of 1 or less
; *** Exposure Effect Module ***
  conc.nectar      ; ug/g; daily concentration of a.i. in nectar (from file)
  conc.pollen      ; ug/g; daily concentration of a.i. in pollen (from file)
  conc.spray       ; ug/g; daily concentration of a.i. in direct spray (from file)
  conc.mat         ; ug/g; daily concentration of a.i. in soil for cell partitions or nest matrix (from file)
  conc.leaf        ; ug/cm2; daily concentration of a.i. on the surface of leaves used for nest lining (from file)
  ThreshList_IT    ; list of thresholds for GUTS-IT: log logistic distribution of individual thresholds; each new individuals chooses a threshold from this list
  deaths.exp.in    ; yearly cumulative number of deaths at time of emergence due to exposure
  deaths.exp.ad.today  ; daily number of adult (emerged and nesting) deaths due to exposure
  deaths.exp.ad    ; total number of adult (emerged and nesting) deaths due to exposure to date
  sum.exposure.in  ; sum of in-nest exposures of all bees emerging today ; for testing
  cnt.em           ; counter of bees reaching transition to emergence (includes bees that die at time of emergence) ; for testing
  exposure.in      ; ug/g; average residue concentration in in-nest life stages ; for testing
  C.effective.today    ; ng/bee; average effective concentration at the start of the day across adult females ; for testing
; ***
]

turtles-own [
  nest.strategy    ; nesting strategy (below, above, leafcutting) according to species
  sex              ; sex of bee (female, male)
  life.stage       ; life stage of the bee (egg, larva, cocoon, emerged, nesting)
                   ;    cocoon summarizes all in-nest stages from pre-pupa to pre-emergent adult
  hatch.year       ; year when the bee agent was created (egg laid); for univoltine life cycles,
                   ;    this is used to make sure the bee does not emerge in the same year
  total.age        ; age since laid as egg (days); included for testing
  age.stage        ; age in current life stage (days)
  emerge.doy       ; day of year of emergence
  mort.prob        ; daily probability of death
  cell.prod        ; daily production rate of cells (nesting females only)
  f.cells.today    ; number of cells with female offspring compelted each day
  m.cells.today    ; number of cells with male offspring compelted each day
  f.cell.cnt       ; total number of cells with female offspring built
  m.cell.cnt       ; total number of cells with male offspring built
  prov.today       ; relative provision collected today (calculated from input)
  sex.ratio        ; daily ratio of female cells produced (number of female cells / number of all cells)
  prov.size        ; relative provision size (proportion of maximum size for female offspring)
  nest.y.cor       ; for interface only: y-coordinate of nest in 'world' (all cells produced by one female shown in one 'nest')
; *** Exposure Effect Module ***
  c.provision      ; ug/g; residue concentration in food provision
  c.nest.soil      ; ug/g; residue in cell partition (cavity-nesting bees, e.g., Osmia) or soil nest matrix (soil-nesting bees) at time of brood cell completion
  c.nest.leaf      ; ug/cm2; residue on leaves lining the brood cell (in leaf-cutting bees, i.e., Megachile)
  d.since.spray    ; d; number of days since the last direct spray occurred
  t.effects_IT     ; ng/bee; individual threshold for survival (GUTS-IT)
  C.stomach        ; ng/bee; pesticide concentration in stomach content (for consumption; excludes nectar and pollen for brood provisioning)
  C.direct.spray   ; ng/bee; exposure of adult bee direct spray
  C.contact.mat    ; ng/bee; exposure of adult bee from contact with exposed nesting material or matrix
  C.effective      ; ng/bee; effective (external) concentration, combination of oral and contact exposure
  C.effective.ini  ; ng/bee; effective concentration at the start of the simulated day ; for testing
  C_i              ; ng/bee; internal concentration (GUTS)
; ***
]

to setup
  clear-all
  reset-ticks

  ; setting random number seed for the simulation
  random-seed RndSeed

  ; reading input file (floarl input scenario)
  ifelse (MultiYearInput = TRUE) ; input with scenarios for multiple years (list of files)
  [
    if (file-exists? List.input.floral = FALSE) [
      user-message "Warning: List of floral input files does not exist in the current directory!"
      print "setup: Warning: List of floral input files does not exist in the current directory!"
      stop
    ]
    set list.input.files []
    set list.DD.thresh []
    set list.DD.max.cells []
    let input.file csv:from-file List.input.floral ; reading input file
    set sim.yrs (length input.file) - 1 ; note that the csv input contains columns headers (not read in the following)
;    type "setup: reading input file, sim.yrs: " print sim.yrs ; for testing
;    type "setup: reading input file, content: " print input.file ; for testing
    let cnt 1
    repeat sim.yrs [ ; read in all floral input file names and DD parameters into separate lists
      let f.tmp item cnt input.file
      set list.input.files lput (item 0 f.tmp) list.input.files    ; floral input scenarios (yearly time series)
      set list.DD.thresh lput (item 1 f.tmp) list.DD.thresh        ; DD.thresh for each simulated year
      set list.DD.max.cells lput (item 2 f.tmp) list.DD.max.cells  ; DD.max.cells for each simulated year
      set cnt cnt + 1
    ]
;    type "setup: reading input file, file list: " print list.input.files ; for testing
;    type "setup: reading input file, DD.thres list: " print list.DD.thresh ; for testing
;    type "setup: reading input file, DD.max.cells list: " print list.DD.max.cells ; for testing
    let file.num 0
    repeat length list.input.files
    [
      if (file-exists? (item file.num list.input.files) = FALSE)
      [
        type "Floral input file number " type file.num type ", " type item file.num list.input.files print ", does not exist in the current directory."
        user-message "Warning: Floral input file does not exist in the current directory!"
        stop
      ]
      set file.num file.num + 1
    ]
    ; reading first input file
    set f.list csv:from-file (item 0 list.input.files)
;    type "setup: first floral file: " print item 0 list.input.files ; for testing
    ; check if file length is correct (expecting a list of 366 item: header and one row for each day of the year)
    if (length f.list != 366) [
      user-message "Warning: Floral input file has the wrong length (expects 365 entries in each column)!"
      print "setup: Warning: Floral input file has the wrong length (expects 365 entries in each column)!"
      stop
    ]
    ; set DD parameters for first simulated year
    set DD.thresh item 0 list.DD.thresh
    set DD.max.cells item 0 list.DD.max.cells
;    type "setup: multi-year input; for first year: DD.thresh: " type DD.thresh type ", DD.max.cells: " print DD.max.cells ; for testing
  ] [ ; input for a single year scenario only (scenario is repeated in each simulated year)
    set sim.yrs Num.repeat.yr
    ; check the existance of the input file
    if (file-exists? input.floral = FALSE) [
      user-message "Warning: Floral input file does not exist in the current directory!"
      print "setup: Warning: Floral input file does not exist in the current directory!"
      stop
    ]
    ; read input file to list of lists
    set f.list csv:from-file input.floral
    ; check if file length is correct (expecting a list with 366 items: header and one row for each day of the year)
    if (length f.list != 366) [
      user-message "Warning: Floral input file has the wrong length (expects 365 entries in each column)!"
      print "setup: Warning: Floral input file has the wrong length (expects 365 entries in each column)!"
      stop
    ]
    ; the input file determines the number of years simulated (first column is the day of year, all following columns define
    ;  the daily quality of the floral resources for each simulated year)
    set DD.thresh DD.thresh.s ; from interface
    set DD.max.cells DD.max.cells.s ; from interface
  ]

  ; bee agents set their emergence date within the range identified on the interface:
  if ((day.emerge.f + var.emerge.f) > latest.emerge OR (day.emerge.m + var.emerge.m) > latest.emerge) [
      user-message "Warning: emergence of individual bee can occur after 'latest.emerge': these agents set to die."
      print "setup: Warning: emergence of individual bee can occur after 'latest.emerge': these agents set to die."
  ]

  ; counter of total number of bees emerged in a simulted year
  set bees.emerged.yr 0
  set f.emerged.yr 0
  set m.emerged.yr 0
  set f.postemergent.today 0
  ; counter of total number of female bees reaching nesting stage in a simulated year
  set bees.nesting 0
  ; counter of daily number of nesting females
  set bees.nesting.today 0

  ; *** Exposure Effect Module ***
  ; exposure concentrations from floral resource patch set to 0 initially (daily concentrations read from file)
  set conc.nectar 0
  set conc.pollen 0
  set conc.spray 0
  set conc.mat 0
  set conc.leaf 0
  set sum.exposure.in 0  ; for testing
  set cnt.em 0  ; for testing
  set exposure.in 0  ; for testing'
  if Effects = TRUE [
    if GUTS = "NoEffect" [ print "Setting to no effects on adult bees (no GUTS applied) - model setting for testing effects on in-nest life stages only." ]
    if GUTS = "GUTS-IT" [ with-local-randomness [ set ThreshList_IT GUTS_IT_tresholds ] ]
  ]
  ; ***

  ; set the day of year that corresponds to the defined start of the simulation
  set doy Start.day
  tick-advance doy - 1
  ifelse (Start.day = 1)
  [ set year 0 ] ; if the simulation starts on 1 January (doy = 1), the year is increased to year = 1 on the first day of the simulations
  [ set year 1 ] ; if the simulation starts later than 1 January (doy = 1), the year set here is the first simulated year

  ; set up initial number of female bees
  ; the life stage is determined by Initial.stage
  create-turtles Initial.num.f [
    ; set appearence on interface 'world'
    set xcor random-xcor
    set shape "bug"
    set size 0.5

    ; set nesting stratgy according to simulated species
    set nest.strategy species.strategy
    set sex "female"

    ; initial stage and age set on interface: checking here whether the two inputs are aligned
    set life.stage initial.stage
    if life.stage = "cocoon" [
      if initial.age > 365 [
        type "Intial age " type "initial.age" print " of cocoons exceeds maximum age of cocoons (365 days)."
        user-message "WARNING: invalid Initial.age of cocoons set."
        stop
      ]
      set ycor 3
      set hatch.year 0 ; assumption that bee was laid as egg in previous year
      set age.stage initial.age ; cocoon age does not need to vary because emergence is tied to date (doy) rather than age
      set total.age age.stage + dev.egg + dev.larva
    ]
    if life.stage = "emerged" [
      if initial.age >= t.maturation [
        type "Intial age " type "initial.age" type " of emerged bees exceeds time of " type t.maturation print " days to maturation."
        user-message "WARNING: invalid Initial.age of emerged bees set."
        stop
      ]
      set ycor 4
      set hatch.year 0 ; assumption that bee was laid as egg in previous year
      set age.stage random (initial.age + 1) ; setting of initial emerged age: random number between 0 and entered age + 1 because time in emerged life stage is short
      set total.age age.stage + dev.egg + dev.larva + dev.cocoon
    ]
    if life.stage = "nesting" [
      if initial.age > max.nesting.life [
        type "Intial age " type "initial.age" type " of nesting bees exceeds maximum life span of " print max.nesting.life
        user-message "WARNING: invalid Initial.age of nesting set."
        stop
      ]
      set ycor 4
      set hatch.year 0 ; assumption that bee was laid as egg in previous year
      set age.stage round (random-normal initial.age var.emerge.f) ; setting range of ages with standard deviation corresponding to range of emergence dates
      if age.stage < 0 [ set age.stage 0 ]
      set total.age age.stage + dev.egg + dev.larva + dev.cocoon + t.maturation
    ]
    if life.stage = "egg" [
      if initial.age > dev.egg [
        type "Intial age " type "initial.age" type " of eggs exceeds egg development time of " print dev.egg
        user-message "WARNING: invalid Initial.age of eggs set."
        stop
      ]
      set ycor 3
      set hatch.year 1 ; assumption that bee was laid as egg in starting year
      set age.stage round (random-normal initial.age (dev.egg / 4)) ; setting range of ages with standard deviation corresponding to 1/4 of the total egg development time
      if age.stage < 0 [ set age.stage 0 ]
      set total.age age.stage
    ]
    if life.stage = "larva" [
      if initial.age > dev.larva [
        type "Intial age " type "initial.age" type " of larvae exceeds larval development time of " print dev.larva
        user-message "WARNING: invalid Initial.age larvae set."
        stop
      ]
      set ycor 3
      set hatch.year 1 ; assumption that bee was laid as egg in starting year
      set age.stage round (random-normal initial.age (dev.larva / 4)) ; setting range of ages with standard deviation corresponding to 1/4 of the total larval development time
      if age.stage < 0 [ set age.stage 0 ]
      set total.age age.stage + dev.egg
    ]
    set emerge.doy round (random-normal day.emerge.f var.emerge.f) ; note that the initial setting of the model assumes emergence during the time window of emergence of bees that overwintered,
                                                                   ; not the second generation in a single season (applies only to multivoltine life cycles)
    if (emerge.doy > latest.emerge) [
      print "WARNING (setup): emerge date set after latest.emerge: bee (egg) died."
      die
    ]
    set mort.prob 0
    set cell.prod 0
    set f.cells.today 0
    set m.cells.today 0
    set f.cell.cnt 0
    set m.cell.cnt 0
    set sex.ratio -1
    set prov.size 1
    set nest.y.cor 3
    ; *** Exposure Effect Module ***
    if Effects = TRUE [
      if GUTS = "GUTS-IT" [
        with-local-randomness [
          let p round ((random-float 1) * 1000)
          ifelse (p = 1000)
          [ set t.effects_IT item 999 ThreshList_IT ]
          [ set t.effects_IT item p ThreshList_IT ]
;      type p type ", conc: " print guts_it_thresh ; for testing
        ]
      ]
      set c.provision 0
      set c.nest.soil 0
      set c.nest.leaf 0
      set C.effective 0
      set C.effective.ini 0 ; unused in males ; for testing
      set C_i 0
    ]
    ; ***
  ]

  ; set up initial number of male bees
  ; the life stage is determined by Initial.stage
  create-turtles Initial.num.m [
    ; set appearence on interface 'world'
    set xcor random-xcor
    set shape "bug"
    set size 0.3

    ; set nesting stratgy according to simulated species
    set nest.strategy species.strategy
    set sex "male"

    set life.stage initial.stage
    if life.stage = "cocoon" [
      if initial.age > 365 [
        type "Intial age " type "initial.age" print " of cocoons exceeds maximum age of cocoons (365 days)."
        user-message "WARNING: invalid Initial.age of cocoons set."
        stop
      ]
      set ycor 3
      set hatch.year 0 ; assumption that bee was laid as egg in previous year
      set age.stage initial.age ; cocoon age does not need to vary because emergence is tied to date (doy) rather than age
      set total.age age.stage + dev.egg + dev.larva
    ]
    if life.stage = "emerged" [
      if initial.age >= m.life [
        type "Intial age " type "initial.age" type " of emerged bees exceeds time of " type m.life print " male post-emergent life span."
        user-message "WARNING: invalid Initial.age of emerged bees set."
        stop
      ]
      set ycor 5
      set hatch.year 0 ; assumption that bee was laid as egg in previous year
      set age.stage random (initial.age + 1) ; setting of initial emerged age: random number between 0 and entered age + 1 because time in emerged life stage is short
      set total.age age.stage + dev.egg + dev.larva + dev.cocoon
    ]
    if life.stage = "nesting" [
        print "Males in the model die after life stage 'emerged': cannot create male bees in nesting stage."
        user-message "WARNING: Cannot create male bees in nesting stage."
        stop
    ]
    if life.stage = "egg" [
      if initial.age > dev.egg [
        type "Intial age " type "initial.age" type " of eggs exceeds egg development time of " print dev.egg
        user-message "WARNING: invalid Initial.age of eggs set."
        stop
      ]
      set ycor 3
      set hatch.year 1 ; assumption that bee was laid as egg in starting year
      set age.stage round (random-normal initial.age (dev.egg / 4)) ; setting range of ages with standard deviation corresponding to 1/4 of the total egg development time
      if age.stage < 0 [ set age.stage 0 ]
      set total.age age.stage
    ]
    if life.stage = "larva" [
      if initial.age > dev.larva [
        type "Intial age " type "initial.age" type " of larvae exceeds larval development time of " print dev.larva
        user-message "WARNING: invalid Initial.age larvae set."
        stop
      ]
      set ycor 3
      set hatch.year 1 ; assumption that bee was laid as egg in starting year
      set age.stage round (random-normal initial.age (dev.larva / 4)) ; setting range of ages with standard deviation corresponding to 1/4 of the total larval development time
      if age.stage < 0 [ set age.stage 0 ]
      set total.age age.stage + dev.egg
    ]
    set emerge.doy round (random-normal day.emerge.m var.emerge.m)
    if (emerge.doy > latest.emerge) [
      print "WARNING (setup): emerge date set after latest.emerge: bee (egg) died."
      die
    ]
    set mort.prob 0
    set cell.prod -1 ; unused in males
    set f.cells.today -1 ; unused in males
    set m.cells.today -1 ; unused in males
    set f.cell.cnt -1 ; unused in males
    set m.cell.cnt -1 ; unused in males
    set sex.ratio -1 ; unused in males
    set prov.size 1
    set nest.y.cor 0 ; unused in males
  ; *** Exposure Effect Module ***
    if Effects = TRUE [
      if GUTS = "GUTS-IT" [
        with-local-randomness [
          let p round ((random-float 1) * 1000)
          ifelse (p = 1000)
          [ set t.effects_IT item 999 ThreshList_IT ]
          [ set t.effects_IT item p ThreshList_IT ]
;      type p type ", conc: " print guts_it_thresh
        ]
      ]
      set c.provision 0
      set c.nest.soil 0
      set c.nest.leaf 0
;      set C.effective 0 ; unused in males
      set C_i 0
    ]
    ; ***
  ]

  ;; summary model output variables
  set sum.cells.today 0
  set sum.f.cells.today 0
  set sum.m.cells.today 0
  set sum.cells 0
  set sum.f.cells 0
  set sum.m.cells 0
  set mean.cells.today 0
  set mean.f.cells.today 0
  set mean.m.cells.today 0
  set mean.cells 0
  set mean.f.cells 0
  set mean.m.cells 0
  ; *** Exposure Effects Module
  set deaths.exp.in 0
  set deaths.exp.ad.today 0
  set deaths.exp.ad 0
  set C.effective.today 0 ; for testing
  ; ***
end

; called in every time step (main control procedure)
to go
  ; doy = day of year or Julian day
  set doy (ticks mod 365) + 1
  ; reset daily production counts and means
  set sum.cells.today 0
  set sum.f.cells.today 0
  set sum.m.cells.today 0
  set mean.cells.today 0
  set mean.f.cells.today 0
  set mean.m.cells.today 0
  set prov.crop.today 0
  set prov.nat.today 0
  ; reset mean cells per female at the start of each simulated year
  if doy = 1 [
    set year year + 1
    if year > sim.yrs [
      print "go: Simulation reached the end, defined by Num.repeat.yr or set by the input file List.input.floral. Click Setup to start a new simulation."
      set year sim.yrs + 1
      stop
    ] ; simulation stops when all years defined by the input file have been simulated

    set bees.emerged.yr 0
    set f.emerged.yr 0
    set m.emerged.yr 0
    set bees.nesting 0
    set sum.cells 0
    set sum.f.cells 0
    set sum.m.cells 0
    set mean.cells 0
    set mean.f.cells 0
    set mean.m.cells 0
    set deaths.exp.in 0
    set deaths.exp.ad 0
    set sum.exposure.in 0  ; for testing
    set cnt.em 0 ; for testing
    set exposure.in 0 ; for testing

    if (MultiYearInput = TRUE) [ ; at the beginning of each simulated year, read in the next yearly floral scenario file from the list
      set f.list csv:from-file (item (year - 1) list.input.files)
 ;     type "go: first day of year; floral input: " print item (year - 1) list.input.files ; for testing
      ; check if file length is correct (expecting a list of 366 item: header and one row for each day of the year)
      if (length f.list != 366) [
        user-message "Warning: Floral input file has the wrong length (expects 365 entries in each column)!"
        type "go: Warning: Floral input file from year " type year print " has the wrong length (expects 365 entries in each column)!"
        stop
      ]
      set DD.thresh item (year - 1) list.DD.thresh
      set DD.max.cells item (year - 1) list.DD.max.cells
    ]
    ; the input file list determines the number of years simulated
    ; each input file is a time series for one year (365 days: first column is the day of year, all following columns define
    ;  the daily quality of the floral resources for each simulated year)
;    type "go: first day of year; DD.thresh: " type DD.thresh type ", DD.max.cells: " print DD.max.cells ; for testing
  ]

  ; set daily value of floral resource quality (from matrix read from file)
  let f.tmp item doy f.list
  set prop.forag.day item 1 f.tmp
  if (prop.forag.day < 0 OR prop.forag.day > 1) [
    user-message "WARNING: Proportional foraging (prop.forag.day) read from input is outside of defined value range."
    print "go: WARNING: Proportional foraging (prop.forag.day) read from input is outside of defined value range."
  ]
  set Q.crop item 2 f.tmp
  if (Q.crop < 0 OR Q.crop > 1) [
    user-message "WARNING: Resource availability from crop (Q.crop) read from input is input outside of defined value range."
    print "go: WARNING: Resource availability from crop (Q.crop) read from input is input outside of defined value range."
  ]
  set Q.nat item 3 f.tmp
  if (Q.nat < 0 OR Q.nat > 1) [
    user-message "WARNING: Resource availability from off-crop (Q.nat) read from input is input outside of defined value range."
    print "go: WARNING: Resource availability from off-crop (Q.nat) read from input is input outside of defined value range."
  ]
  set crop.forag item 4 f.tmp
  if (crop.forag < 0 OR crop.forag > 1) [
    user-message "WARNING: Proportional foraging on crop (crop.forag) read from input is input outside of defined value range."
    print "go: WARNING: Proportional foraging on crop (crop.forag) read from input is input outside of defined value range."
  ]
  ; *** Exposure Effects Module
  if (Effects = TRUE) [
    set conc.nectar item 5 f.tmp
    set conc.pollen item 6 f.tmp
    set conc.spray item 7 f.tmp
    set conc.mat item 8 f.tmp
    set conc.leaf item 9 f.tmp
  ]
  ; exposure-caused adult (emerged and nesting) death counter reset every day
  set deaths.exp.ad.today 0
  ; ***

  ; daily updates of bee developmental stage, mortality applied, nesting
  ; ODD - Submodel 1 "Life cycle"
  ask turtles [ life.cycle ]

  ; for summary interface plots and outputs:
  set bees.nesting.today count turtles with [ life.stage = "nesting" ]
  set f.postemergent.today bees.nesting.today + count turtles with [ sex = "female" AND life.stage = "emerged" ]
  set bees.emerged.yr f.emerged.yr + m.emerged.yr

  ifelse bees.nesting.today > 0 [
    set mean.cells.today mean [f.cells.today + m.cells.today] of turtles with [life.stage = "nesting"]
    set mean.f.cells.today mean [f.cells.today] of turtles with [life.stage = "nesting"]
    set mean.m.cells.today mean [m.cells.today] of turtles with [life.stage = "nesting"]
  ][
    set sum.cells.today 0
    set sum.f.cells.today 0
    set sum.m.cells.today 0
    set mean.cells.today 0
    set mean.f.cells.today 0
    set mean.m.cells.today 0
  ]
  if bees.nesting > 0 [
    set mean.cells sum.cells / bees.nesting
    set mean.f.cells sum.f.cells / bees.nesting
    set mean.m.cells sum.m.cells / bees.nesting
  ]

  ; *** Exposure Effects Module
  if (Effects = TRUE) [ ; for testing
    with-local-randomness [
      ifelse any? turtles with [life.stage = "nesting"]
      [ set C.effective.today mean [C.effective.ini] of turtles with [life.stage = "nesting"] ]
      [ set C.effective.today 0 ]
      if sum.exposure.in > 0
      [ set exposure.in sum.exposure.in / cnt.em ]
    ]
  ]
  ; ***
  tick
end

; ODD - Submodel 1 "Life cycle"
; called in every time step: implements individual's aging, stage transitions and background mortality
to life.cycle
  if life.stage = "egg" [
    if age.stage > dev.egg [
      set life.stage "larva"
      set age.stage 0
    ]
  ]
  if life.stage = "larva" [
    if age.stage > dev.larva [
      set life.stage "cocoon"
      set age.stage 0
    ]
  ]
  ; background mortality for all in-nest life stages applied at time of emergence from nest
  if life.stage = "cocoon" [
    if (year > hatch.year AND doy > latest.emerge) [ ; kill bee if not emerged at the end of the emergence period (important for simulations of semi-field studies)
      type "life.cycle: Warning (simulated date: doy " type doy type ", year " type year type "): bee agent " type self type " did not emerge this season; forced agent death. Agent's emerge.doy: " print emerge.doy
      die
    ]
    if doy = emerge.doy [
      if (hatch.year < year OR Voltinism = "multivoltine") [
        if (age.stage < dev.cocoon) [
          type "life.cycle; bee " type self type " emerged before completing development, cocoon age: " print age.stage
        ]
        if sex = "female" [
          if (random-float 1) > (max.survival.e.f * prov.size) [ die ]
          set f.emerged.yr f.emerged.yr + 1
          set life.stage "emerged"
          set xcor random-xcor
          set ycor 4
          set shape "bee"
          set age.stage 0
        ]
        if sex = "male" [
          if (random-float 1) > (max.survival.e.m * prov.size) [ die ]
          set m.emerged.yr m.emerged.yr + 1
          set life.stage "emerged"
          set xcor random-xcor
          set ycor 5
          set shape "bee"
          set age.stage 0
        ]
      ]
    ]
  ]
  if life.stage = "emerged" [
    ; bees in life stage "emerged" are assumed not to have a daily background mortality
    ; female emerged bees have a probability to die at the end of the life stage
    ;  (interface input "emerged.survival" reflecting death after emergence and before nesting, failure to nest, dispersal)
    if sex = "female" [
      ; *** Exposure Effect Module: exposure in emerged bees only from food consumption and direct spray ***
      ; effects only implemented for females
      if Effects = TRUE [ with-local-randomness [ effect.adult ] ]
      ; ***
      if age.stage >= t.maturation [
        if (random-float 1) > emerged.survival [ die ]
        set life.stage "nesting"
        set bees.nesting bees.nesting + 1
        set age.stage 0
      ]
    ]
    ; male emerged bees die at the end of their defined life span (m.life)
    if sex = "male" [
      if age.stage >= m.life [ die ]
    ]
  ]
  if life.stage = "nesting" [
    set f.cells.today 0
    set m.cells.today 0
    if age.stage > max.nesting.life [ die ] ; bee reached its maximum life span
                                            ; all emerged adults (emerged and nesting) have a probability of death
                                            ; the daily probability of death is assumed to be independent of post-emergent age
                                            ; the assumed average survival rate to the reported maximum post-emergent life span (max.adult.life) is 1%
    ; *** Exposure Effect Module ***
    if Effects = TRUE [ with-local-randomness [ effect.adult ] ]
    ; ***
    ; Nest building submodel: daily cell production
    nest.building
    ; Daily mortality
    if (random-float 1) > p.max.nesting.life ^ (1 / max.nesting.life) [ die ]
  ]
  set age.stage age.stage + 1
  set total.age total.age + 1
end

; ODD - Submodel 2: Reproduction (nest building)
; called in every time step by nesting females only
; Cell production rate, offspring sex ratio and relative provision size
;   is dependent on daily floral input scenario and female age
; If density dependence is 'on', applied to cell production rate (calling DD.nesting)
to nest.building
  ; calculate relative daily provision collection (provisions collected dependent on floral input scenario)
  calc.provision
  ; cell production rate today
  let t.cell.prod cell.prod.rate
  ; apply density dependence of cell production rate (dependent on total number of individuals in in-nest life stages)
  if Density.dep = TRUE [ set t.cell.prod t.cell.prod * DD.nesting ]
  ; today's total number of cells produced (including the 'unfinished' cell from previous day)
  set cell.prod cell.prod + t.cell.prod
  ; sex ratio of cells (offspring) produced (female/total)
  set sex.ratio cell.sex.ratio
  ; relative provision size (applies to both sexes)
  set prov.size cell.prov.size

  ; new bees are created accordingly
  ; Note that all turtles-own variables not set by 'hatch' are inherited from the mother bee:
  ;   nesting.strategy
  ;   prov.size (daily relative provision size per cell)
  repeat floor cell.prod [
    ifelse (random-float 1) < sex.ratio [   ; female offspring
      hatch 1 [
        set ycor nest.y.cor
        set shape "bug"
        set size 0.5
        set sex "female"
        set life.stage "egg"
        set hatch.year year
        set total.age 0
        set age.stage 0
        ; emergence date is chosen by each individual bee:
        ;  in case of univoltine life cycles, the bee chooses an emergence date (day of year) in the following year
        ;  in case of multivoltine life cycles: if the bee can complete its development before the lastest emergence date,
        ;  it will emerge the same year at the time of completion of its development
        ;  otherwise, it will emerge the following year
        ifelse (Voltinism = "univoltine")
        [ set emerge.doy round (random-normal day.emerge.f var.emerge.f) ]
        [
          ifelse (doy + dev.egg  + 1 + dev.larva  + 1 + dev.cocoon + 1 < latest.emerge)
          [ set emerge.doy doy + dev.egg + 1 + dev.larva + 1 + dev.cocoon + 1 ]
          [ set emerge.doy round (random-normal day.emerge.f var.emerge.f) ]
        ]
        if (emerge.doy > latest.emerge) [
          print "WARNING (life.cycle): emerge date set after latest.emerge: bee (egg) died."
          die
        ]
        set mort.prob 0
        set cell.prod 0
        set f.cells.today 0
        set m.cells.today 0
        set f.cell.cnt 0
        set m.cell.cnt 0
        set prov.today 0
        set sex.ratio -1
        set nest.y.cor 3
        ; *** Exposure Effect Module ***
        if Effects = TRUE [
          if GUTS = "GUTS-IT" [
            with-local-randomness [
              let p round ((random-float 1) * 1000)
              ifelse (p = 1000)
              [ set t.effects_IT item 999 ThreshList_IT ]
              [ set t.effects_IT item p ThreshList_IT ]
              ;      type p type ", conc: " print guts_it_thresh
            ]
          ]
          ; Note that c.provision is 'inherited' from the mother bee agent
          set c.nest.soil conc.mat ; ug/g
          set c.nest.leaf conc.leaf ; ug/cm2
          set d.since.spray -1
          set C.stomach 0
          set C.direct.spray 0
          set C.contact.mat 0
          set C.effective 0
          set C.effective.ini 0 ; for testing
          set C_i 0
        ]
        ; ***
      ]
      set nest.y.cor nest.y.cor - 0.5
      set f.cells.today f.cells.today + 1
      set f.cell.cnt f.cell.cnt + 1
      set sum.f.cells.today sum.f.cells.today + 1
      set sum.cells.today sum.cells.today + 1
      set sum.f.cells sum.f.cells + 1
      set sum.cells sum.cells + 1
    ] [  ; male offspring
      hatch 1 [
        set ycor nest.y.cor
        set shape "bug"
        set size 0.3
        set sex "male"
        set life.stage "egg"
        set hatch.year year
        set total.age 0
        set age.stage 0
        ; emergence date is chosen by each individual bee:
        ;  in case of univoltine life cycles, the bee chooses an emergence date (day of year) in the following year
        ;  in case of multivoltine life cycles: if the bee can complete its development before the lastest emergence date,
        ;  it will emerge the same year at the time of completion of its development
        ;  otherwise, it will emerge the following year
        ifelse (Voltinism = "univoltine")
        [ set emerge.doy round (random-normal day.emerge.m var.emerge.m) ]
        [
          ifelse (doy + dev.egg  + 1 + dev.larva  + 1 + dev.cocoon + 1 < latest.emerge)
          [ set emerge.doy doy + dev.egg + 1 + dev.larva + 1 + dev.cocoon + 1 ]
          [ set emerge.doy round (random-normal day.emerge.m var.emerge.m) ]
        ]
        if (emerge.doy > latest.emerge) [
          print "WARNING (life.cycle): emerge date set after latest.emerge: bee (egg) died."
          die
        ]
        set mort.prob 0
        set cell.prod 0
        set f.cells.today 0
        set m.cells.today 0
        set f.cell.cnt 0
        set m.cell.cnt 0
        set prov.today 0
        set sex.ratio -1
        set nest.y.cor 6
        ; *** Exposure Effect Module ***
        if Effects = TRUE [
          if GUTS = "GUTS-IT" [
            with-local-randomness [
              let p round ((random-float 1) * 1000)
              ifelse (p = 1000)
              [ set t.effects_IT item 999 ThreshList_IT ]
              [ set t.effects_IT item p ThreshList_IT ]
              ;      type p type ", conc: " print guts_it_thresh
            ]
          ]
          ; Note that c.provision is 'inherited' from the mother bee agent
          set c.nest.soil conc.mat ; ug/g
          set c.nest.leaf conc.leaf ; ug/cm2
          set d.since.spray -1 ; unused in males
          set C.stomach 0 ; unused in males
          set C.direct.spray 0 ; unused in males
          set C.contact.mat 0 ; unused in males
          set C.effective 0 ; unused in males
          set C.effective.ini 0 ; unused in males ; for testing
          set C_i 0 ; unused in males
        ]
        ; ***
      ]
      set nest.y.cor nest.y.cor - 0.3
      set m.cells.today m.cells.today + 1
      set m.cell.cnt m.cell.cnt + 1
      set sum.m.cells.today sum.m.cells.today + 1
      set sum.cells.today sum.cells.today + 1
      set sum.m.cells sum.m.cells + 1
      set sum.cells sum.cells + 1

    ]
  ]
  set cell.prod cell.prod - (floor cell.prod)
end

; called by nesting females (in procedure nest.building)
; calculates the effective provision collection on a given day (as number of cell provisions),
;  dependent on resource availability / quality (from floral input scenario)
to calc.provision
  ; set relative forage availability to the effective foraging time available today
  let rel.Q prop.forag.day
  ; if using stochastic proportion of foraging on crop (set on interface)
  if (stoch.crop.forag = TRUE) [
    ; stochastic proportion only set if an input value other than 0 (no crop foraging) or 1 (only foraging on crop) is set
    if (crop.forag > 0 AND crop.forag < 1) [
      with-local-randomness [ set crop.forag random-float 1 ] ; crop.forag set to random number between 0 and 1
    ]
  ]
  ; relative amount of collected provision sourced from crop
  set prov.crop.today rel.Q * crop.forag
  ; relative amount of collected provision sourced off-crop
  set prov.nat.today rel.Q - prov.crop.today
  ; resource availability from crop
  set prov.crop.today prov.crop.today * Q.crop
  ; resource availability from off-crop
  set prov.nat.today prov.nat.today * Q.nat
  ; effective provision amount (number of provisions) collected today
  set prov.today prov.crop.today + prov.nat.today
;;  type "calc.provision, Turtle: " type self type ", prov.today: " print prov.today ; for testing

  ; *** Exposure Effects Module ***
  ; calculate exposure concentration in provision
  if (Effects = TRUE) [
    ; considering provision composition (pollen vs. nectar)
    ; nectar_prop is an input defining the proportion of nectar in the total provision by weight;
    ;  assumption that the remainder of the provision is pollen;
    ;  requires that concentrations in nectar and pollen are both given by weight (ug a.i./g nectar or pollen)
    let c.crop ((nectar_prop * conc.nectar) + ((1 - nectar_prop) * conc.pollen))
    ; residue in provision dependent on proportion of resources collected from crop
    set c.provision 0
    if (prov.today > 0) [ set c.provision c.crop * (prov.crop.today / prov.today) ]
  ]
  ; ***
end

; calculates daily total rate of cells built (dependent on female age and floral resource quality); includes cells with female and male offspring
to-report cell.prod.rate
  let cell.age (a.cell.age * age.stage + 1)
  let cell.res 0
  ; no cell production on days without foarging/ floral resource availability
  if (prov.today > 0) [ set cell.res (a.cell.resource * prov.today + (1 - a.cell.resource)) ]

  report (max.cells * cell.age * cell.res)
end

; calculate daily sex ratio of cells produced (female cells / all cells)
to-report cell.sex.ratio
  let sex.age (a.sex.age * age.stage + 1)
  let sex.res (a.sex.resource * prov.today + (1 - a.sex.resource))

  let f.r max.f.ratio * sex.age * sex.res
  if f.r < 0 [ set f.r 0 ]
  report f.r
end

; calculate relative cell provision size (1 = maximum size; 0 = no provision)
to-report cell.prov.size
  let size.age (a.size.age * age.stage + 1)
  let size.res (a.size.resource * prov.today + (1 - a.size.resource))

  report (size.age * size.res)
end

to-report DD.nesting
  ; update number of cells currently occupied (makes sure the cells produced by other females on the same day are also included)
  let occ.cells count turtles with [ life.stage = "egg" or life.stage = "larva" or life.stage = "cocoon" ]

  let rel.cell.prod 1
  if occ.cells > DD.thresh [
    if DD.funct = "linear" [ set rel.cell.prod ((-1) * (occ.cells - DD.thresh) / (DD.max.cells - DD.thresh)) + 1 ]
    if DD.funct = "log"    [ set rel.cell.prod 1 / (1 + exp(ln(occ.cells - DD.thresh) - ln((DD.max.cells - DD.thresh) / 2)) ^ DD.log.slope) ]
  ]
 ; type "DD.nesting: relative cell production rate: " print rel.cell.prod ; for testing

  report rel.cell.prod
end

; set nesting stragey according to the species defined on the interface
to-report species.strategy
  if species = "N.melanderi" [ report "below" ]
  if species = "E.pruinosa" [ report "below" ]
  if species = "M.rotundata" [ report "leafcutting" ]
  if species = "O.bicornis" [ report "above" ]
  if species = "O.cornifrons" [ report "above" ]
  if species = "O.cornuta" [ report "above" ]
  if species = "O.lignaria" [ report "above" ]
end

; ***********************************************************************************************
; *** Exposure and Effect Module ***

; calculate daily effect on survival of post-emergent females
; note that effect on post-emergent males are not simulated
to effect.adult
  set C.effective 0 ; ng/bee; effective concentration (corresponds to external concentration in GUTS)
                    ; is recalculated in each GUTS time step as a sum of exposures from different routes (see below)
                    ; C.effective is the amount (in ng) of compound received per individual, i.e., it corresponds to ng/bee

  ; * Exposure from nectar and pollen consumption *
  ; Nectar and pollen consumption are independent of quality of floral resources
  ; The effective concentration received from oral exposure (due to consumption of exposed nectar and/or pollen) is assumed to correspond
  ;  directly to the residue in the consumed food (C.stomach, in ng/bee).
  ;  This assumption corresponds to the handling of chronic oral toxicity study data in BeeGUTS (not the acute oral study data).
  ;  Note that this results the stomach release rate (kSR) from BeeGUTS is not used and corresponds to the assumption that
  ;  adult females digest all nectar and pollen consumed in one day within 24 h.
  ; The external (effective) concentration derived from oral exposure is assumed to be constant for each day
  ;  (corresponding to the assumptions in BeeGUTS for chronic oral exposure)
  let crop.today prop.forag.day * crop.forag
  ; C.stomach: amount of compound (ng) in stomach (corresponds to ng/bee)
  ;  Note on units: ad.nectar.cons [mg] * conc.nectar [ug/g] = (ad.nectar.cons / 1000) [g] * (conc.nectar * 1000) ng/g; applies to pollen as well
  set C.stomach ((ad.nectar.cons * conc.nectar) + (ad.pollen.cons * conc.pollen)) * crop.today

  ; * Exposure from direct spray *
  ; Compound stays on the bee until all is transferred to the effective concentration over time.
  ; Multiple spray events are additive (if they happen within 10 days, see below).
  ; 10 days after the last exposure to direct spray, C.direct.spray is set to 0 (assumption that all compound is gone from the bee's surface);
  ;  corresponds to the duration simulated by BeeGUTS (Baas et al. 2022)
  ;  C.direct.spray at the start of the day: gets reduced during the day (see guts loop below)
  ; Consideration of probability that a bee is foraging in crop during application
  let in.crop.today random-float 1 ; draw random number between 0 and 1
  ifelse (in.crop.today < prop.forag.day * crop.forag)
	[ set in.crop.today 1 ] ; bee was foraging in crop today
	[ set in.crop.today 0 ] ; bee was not foraging in crop today

  ifelse (conc.spray > 0 AND in.crop.today > 0)[ ; the bee was exposed to direct spray today
    set d.since.spray 0
    set C.direct.spray C.direct.spray + conc.spray ; amount of compound (ng) on the surface of the bee from direct spray;
                                                   ; note that conc.spray is given in mg/L (=ng/uL)
                                                   ; and a single droplet of 1 uL is assumed to hit the bee (corresponding to droplet size in acute contact tests with the honey bee),
                                                   ; residues present from previous day are included;
  ] [
    set d.since.spray d.since.spray + 1
    if d.since.spray > 10 [ ; last direct spray occurred 11 days ago: no residue on bee assumed (first order decline never reaches 0 and would result triggering GUTS despite very low residual concentrations)
      set C.direct.spray 0
      set d.since.spray -1
    ]
  ]
  ; * Exposure from contact to nesting material or matrix (nesting females only) *
  let exp.time.mat 0
  if life.stage = "nesting" [
    if Transfer.mat.adult = TRUE [
      ; C.contact.mat: amount of compound (ng) per bee and day received from contact with exposed nesting matrix or material
      if nest.strategy = "above" [
        ; Note on units: C.contact.mat [ng] = TC [g/d] * conc.mat [ug/g] = TC [g/d] * (conc.mat * 1000) [ng/g]
        set C.contact.mat TC_soil * (conc.mat * 1000) ; contribution of contact to nesting material or matrix to effective concentration
                                                      ; exp.time.mat defines the time (in proportion of a day) that a nesting bee is exposed to nesting material or matrix (used in GUTS below)
        set exp.time.mat t.guts * ad.ET ; time of exposure to soil corresponds to time spent nest building
      ]
      if nest.strategy = "leafcutting" [
        ; Note on units: C.contact.mat [ng] = TC [cm2/d] * conc.mat [ug/cm2] = TC [cm2/d] * (conc.mat * 1000) [ng/g]
        set C.contact.mat TC_leaf * (conc.leaf * 1000)
        set exp.time.mat t.guts * ad.ET ; time of exposure to leaf material for nest lining corresponds to time spent nest building
      ]
      if nest.strategy = "below" [
        ifelse Exposure.resting.soil = TRUE
        [ set exp.time.mat t.guts ] ; assumption that a below-ground nesting bee is exposed all day via nesting matrix (soil)
        [ set exp.time.mat t.guts * ad.ET ]
      ]
    ]
  ]

  ; * Apply effects (BeeGUTS; Baas et al. 2022) *
  let cnt.guts 0 ; for testing
  set C.effective.ini C.stomach + C.contact.mat + C.direct.spray ; for testing
  if (C.stomach + C.contact.mat + C.direct.spray) > 0 [ ; only run GUTS if effective concentration > 0
    repeat t.guts [
      ; Effective (external) concentration is calculated as the sum of compound received from residue in the stomach,
      ;  uptake from direct spray and contact to nesting material or matrix.
      ; Contribution to effective concentration from oral exposure (nectar and pollen consumption): C.stomach
      set C.effective C.stomach
      ; Contribution to effective concentration from direct spray
      if C.direct.spray > 0 [
        set C.direct.spray C.direct.spray * exp((-1) * k_CA / t.guts)
        set C.effective C.effective + C.direct.spray
      ]
      ; Contribution to effective concentration from contact with exposed nesting material or matrix
      ; exposure from contact to nesting material or matrix (nesting bees only): exposure only during contact with the material (defined by ad.ET)
      if exp.time.mat > 0 [
        set C.effective C.effective + C.contact.mat
        set exp.time.mat exp.time.mat - 1
      ]
      if GUTS = "GUTS-SD" [ GUTS_SD C.effective ]
      if GUTS = "GUTS-IT" [ GUTS_IT C.effective ]

;      type self type ", " type doy type "." type cnt.guts type ", " print C.effective ; for testing

      set cnt.guts cnt.guts + 1
    ]
  ]
end

; calculate effects on survival of in-nest life stages applied at time of emergence
to effect.in.nest
 ; type "procedure effect.in.nest " print self ; for testing
  let exposure 0
  ifelse Transfer.mat.dev = FALSE
  ; no transfer of residue from nest matrix/material to developing bee/provision assumed
  [ set exposure c.provision ] ; ug/g; c.provision is calculated upon completion of the provision for the individual in 'calc.provision'; exposure in ng/g provision
  [
    ; ground-nesting bees: residues in soil nest matrix are considered
    if nest.strategy = "below"
    [
      ; full mixing of soil residue with developing bee/provision assumed (e.g., Nomia, Peponapis):
      ;   only considered if the residue concentration in the provisions is lower than in the soil
      ifelse c.provision >= c.nest.soil ; c.nest.soil corresponds to conc.mat on the day of the individual's 'creation' (completion of brood cell)
      [ set exposure c.provision ]
      [ set exposure c.nest.soil ]
    ]
    ; above-ground nesting bees with soil/mud cell partitions (e.g., Osmia):
    ;   residue in one cell partition assumed to mix equally across the partition and the provision
    if nest.strategy = "above"
    [
      let c.temp c.nest.soil * (SM / (SM + weight.prov)) ; c.nest.soil corresponds to conc.mat on the day of the individual's 'creation' (completion of brood cell)
      ifelse c.provision >= c.temp
      [ set exposure c.provision ]
      [ set exposure c.temp ]
    ]
    ; above-ground nesting bees with cell lining made from leaf pieces (e.g. Megachile):
    ;   residue in leaf material assumed to mix equally across the leaf material and the provision
    if nest.strategy = "leafcutting"
    [
      ; Note on units: c.temp [ug/g] = c.nest.leaf [ug/cm2] * F [-] * SA_i [cm2] / weight.prov [mg] = c.nest.leaf [ug/cm2] * F [-] * SA_i [cm2] / (weight.prov / 1000) [g]
      let c.temp (c.nest.leaf * F * SA_i) / weight.prov / 1000
      ifelse c.provision >= c.temp
      [ set exposure c.provision ]
      [ set exposure c.temp ]
    ]
  ]
  set sum.exposure.in sum.exposure.in + exposure ; for testing
  set cnt.em cnt.em + 1 ; for testing
  let effect dose.response.log dr.intercept dr.slope exposure
  if (random-float 1) > effect [
    set deaths.exp.in deaths.exp.in + 1
    die
  ]
end

; calculate dose-response using log-linear function
; returns survival rate
to-report dose.response.log [ intercept slope concentration ]
  let survival 1
  if concentration > 0 [
    set survival intercept + (slope * (log concentration 10)) ; log-linear function is not bound to [0, 1]
    if survival > 1 [ set survival 1 ]
    if survival < 0 [ set survival 0 ]
  ]
  report survival
end

; ***********************************************************************************************

; procedure adapted from BEEHAVE (Becher et al. 2014): displaying calendar date on the interface (not used anywhere else in the code)
to-report DateREP
  let month-names (list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
  let days-in-months (list 31 28 31 30 31 30 31 31 30 31 30 31)
  let month 0
  let dayOfMonth 0
  let sumDaysInMonths 0
  while [ sumDaysInMonths < doy ]
  [
    set month month + 1
    set sumDaysInMonths sumDaysInMonths + item (month - 1) days-in-months
    set dayOfMonth doy - sumDaysInMonths + item (month - 1) days-in-months
  ]

  report (word dayOfMonth "  " (item (month - 1) month-names) " " year )
end

; ***********************************************************************************************

; procedure to check whether model outputs correspond to original SolBeePop_ecotox version
; using O. bicornis settings with dimethoate exposures and effects (2021 semi-field study)
to version.check
  let control.results TRUE
  let effect.results TRUE

  ;; 1) check control simulation (no effects)
  set Start.day 1
  set Species "O.bicornis"
  set Voltinism "univoltine"
  set Initial.num.f 100
set Initial.num.m 200
set Initial.stage "cocoon"
set Initial.age 200
set RndSeed 42
set MultiYearInput FALSE
set List.input.floral "NA"
set Num.repeat.yr 2
set input.floral "Floral_generic_optimal_DIMexp.csv"
set stoch.crop.forag FALSE
set Density.dep FALSE
set DD.thresh.s 250
set DD.max.cells.s 2500
set DD.funct "linear"
set DD.log.slope 2
set day.emerge.f 105
set var.emerge.f 3
set day.emerge.m 91
set var.emerge.m 2
set latest.emerge 365
set dev.egg 8
set dev.larva 32
set dev.cocoon 68
set t.maturation 3
set m.life 14
set max.nesting.life 36
set p.max.nesting.life 0.04
set max.f.ratio 0.59
set max.cells 2
set max.survival.e.f 0.74
set max.survival.e.m 0.74
set emerged.survival 0.544
set a.cell.age -0.006
set a.sex.age -0.0406
set a.size.age -0.003
set a.cell.resource 0.94
set a.sex.resource 0.42
set a.size.resource 0.114
set Effects FALSE
set ad.nectar.cons 208.34
set ad.pollen.cons 11.6
set k_CA 0.4
set Transfer.mat.adult TRUE
set ad.ET 0.072
set TC_soil 0.5
set TC_leaf 0.5
set Exposure.resting.soil FALSE
set GUTS "GUTS-IT"
set t.guts 10
set kd_SD 0.39
set bw_SD 0.014
set mw_SD 13
set kd_IT 0.012
set mw_IT 2.4
set Fs_IT 3
set nectar_prop 0.78
set weight.prov 306
set Transfer.mat.dev TRUE
set SM 187
set F 0
set SA_i 0
set dr.intercept 1.1534
set dr.slope -0.7876
setup
  repeat 365 [go]
  type "control simulation, sum emerged: " type bees.emerged.yr type ", female cells: " type sum.f.cells type ", male cells: " print sum.m.cells
  ifelse (bees.emerged.yr != 229 OR sum.f.cells != 321 OR sum.m.cells != 580)
  [
    set control.results FALSE
    print "Control simulation results without exposures: numbers do not match with original version."
  ]
  [ print "Control simulation results without exposures: numbers match with original version." ]

  ;; 2) check effect simulation
  set Start.day 1
set Species "O.bicornis"
set Voltinism "univoltine"
set Initial.num.f 100
set Initial.num.m 200
set Initial.stage "cocoon"
set Initial.age 200
set RndSeed 42
set MultiYearInput FALSE
set List.input.floral "NA"
set Num.repeat.yr 2
set input.floral "Floral_generic_optimal_DIMexp.csv"
set stoch.crop.forag FALSE
set Density.dep FALSE
set DD.thresh.s 250
set DD.max.cells.s 2500
set DD.funct "linear"
set DD.log.slope 2
set day.emerge.f 105
set var.emerge.f 3
set day.emerge.m 91
set var.emerge.m 2
set latest.emerge 365
set dev.egg 8
set dev.larva 32
set dev.cocoon 68
set t.maturation 3
set m.life 14
set max.nesting.life 36
set p.max.nesting.life 0.04
set max.f.ratio 0.59
set max.cells 2
set max.survival.e.f 0.74
set max.survival.e.m 0.74
set emerged.survival 0.544
set a.cell.age -0.006
set a.sex.age -0.0406
set a.size.age -0.003
set a.cell.resource 0.94
set a.sex.resource 0.42
set a.size.resource 0.114
set Effects TRUE
set ad.nectar.cons 208.34
set ad.pollen.cons 11.6
set k_CA 0.4
set Transfer.mat.adult TRUE
set ad.ET 0.072
set TC_soil 0.5
set TC_leaf 0.5
set Exposure.resting.soil FALSE
set GUTS "GUTS-IT"
set t.guts 10
set kd_SD 0.39
set bw_SD 0.014
set mw_SD 13
set kd_IT 0.012
set mw_IT 2.4
set Fs_IT 3
set nectar_prop 0.78
set weight.prov 306
set Transfer.mat.dev TRUE
set SM 187
set F 0
set SA_i 0
set dr.intercept 1.1534
set dr.slope -0.7876
setup
  repeat 365 [go]

  type "effect sim, sum emerged: " type bees.emerged.yr type ", female cells: " type sum.f.cells type ", male cells: " print sum.m.cells
  ifelse (bees.emerged.yr != 226 OR sum.f.cells != 75 OR sum.m.cells != 58)
  [
    set effect.results FALSE
    print "Effect simulation results with DIM exposures: numbers do not match with original version."
  ]
  [ print "Effect simulation results with DIM exposures: numbers match with original version." ]

  ifelse (control.results = TRUE AND effect.results = TRUE) [
    user-message "Check successful: outputs match with original SolBeePop_ecotox."
    print "Check successful: outputs match with original SolBeePop_ecotox."
  ][
    user-message "Check failed: outputs do not match with original SolBeePop_ecotox."
    print "Check failed: outputs do not match with original SolBeePop_ecotox."
  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
6
10
1024
189
-1
-1
10.0
1
10
1
1
1
0
1
1
1
-50
50
-8
8
0
0
1
ticks
30.0

BUTTON
424
194
506
227
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
506
194
589
227
Run
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
424
227
479
260
1 Day
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
479
227
534
260
1 Month
if ticks = 0 [ go ] ; to set date to 1 January\nlet days-in-months (list 31 28 31 30 31 30 31 31 30 31 30 31)\nlet month 0\nlet dayOfYear remainder ticks 365.01\n  let dayOfMonth 0\n  let sumDaysInMonths 0\n  while [ sumDaysInMonths < dayOfYear ]\n  [\n    set month month + 1 \n    set sumDaysInMonths sumDaysInMonths + item (month - 1) days-in-months \n    set dayOfMonth dayOfYear - sumDaysInMonths + item (month - 1) days-in-months  \n  ]\n\nrepeat item (month - 1) days-in-months [ go ] \n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
534
227
589
260
1 Year
repeat 365 [ go ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
789
568
873
628
day.emerge.f
105.0
1
0
Number

INPUTBOX
789
627
872
687
var.emerge.f
3.0
1
0
Number

INPUTBOX
788
805
872
865
max.nesting.life
36.0
1
0
Number

INPUTBOX
512
411
795
471
input.floral
Floral_generic_optimal_DIMexp.csv
1
0
String

INPUTBOX
425
565
522
625
max.cells
2.0
1
0
Number

INPUTBOX
425
624
522
684
max.f.ratio
0.59
1
0
Number

INPUTBOX
592
568
689
628
max.survival.e.f
0.74
1
0
Number

INPUTBOX
425
687
508
747
a.cell.age
-0.006
1
0
Number

INPUTBOX
425
746
509
806
a.sex.age
-0.0406
1
0
Number

INPUTBOX
508
687
591
747
a.cell.resource
0.94
1
0
Number

MONITOR
424
260
494
305
Julian Day
doy
0
1
11

MONITOR
494
260
589
305
Date & Year
DateREP
0
1
11

INPUTBOX
508
746
591
806
a.sex.resource
0.42
1
0
Number

INPUTBOX
425
805
509
865
a.size.age
-0.003
1
0
Number

INPUTBOX
508
805
591
865
a.size.resource
0.114
1
0
Number

INPUTBOX
595
195
685
255
Initial.num.f
100.0
1
0
Number

INPUTBOX
685
195
775
255
Start.day
1.0
1
0
Number

INPUTBOX
955
568
1028
628
dev.egg
8.0
1
0
Number

INPUTBOX
955
627
1028
687
dev.larva
32.0
1
0
Number

INPUTBOX
788
746
872
806
t.maturation
3.0
1
0
Number

INPUTBOX
941
243
1025
303
RndSeed
42.0
1
0
Number

INPUTBOX
832
687
912
747
latest.emerge
365.0
1
0
Number

INPUTBOX
955
686
1028
746
dev.cocoon
68.0
1
0
Number

PLOT
7
195
414
345
Number of bees
Day
Number
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Total post-emergent adults" 1.0 0 -16777216 true "" "plot count turtles with [life.stage = \"emerged\" or life.stage = \"nesting\"]"
"Total emerged females" 1.0 0 -14439633 true "" "plot f.emerged.yr"
"Total emerged males" 1.0 0 -955883 true "" "plot m.emerged.yr"
"Total nesting" 1.0 0 -13345367 true "" "plot bees.nesting"
"Nesting today" 1.0 0 -5825686 true "" "plot bees.nesting.today"
"Developing bees" 1.0 0 -7500403 true "" "plot count turtles with [life.stage = \"egg\" or life.stage = \"larva\" or life.stage = \"cocoon\"]"

PLOT
7
343
360
493
Cells per female (cumulative)
Day
Cells/female
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Cells per female" 1.0 0 -16777216 true "" "plot mean.cells"
"Female cells" 1.0 0 -14439633 true "" "plot mean.f.cells"
"Male cells" 1.0 0 -955883 true "" "plot mean.m.cells"

INPUTBOX
593
687
690
747
p.max.nesting.life
0.04
1
0
Number

PLOT
7
491
368
641
Mean cells per female per day
Day
Cells completed
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Mean cells" 1.0 0 -16777216 true "" "plot mean.cells.today"
"Mean female cells" 1.0 0 -14439633 true "" "plot mean.f.cells.today"
"Mean male cells" 1.0 0 -955883 true "" "plot mean.m.cells.today"
"Foraging" 1.0 2 -13791810 true "" "plot prop.forag.day"

TEXTBOX
642
358
790
400
Each line in the file lists an input file for each simulted year plus DD parameters\n
11
0.0
1

MONITOR
924
198
1025
243
Simulated years
sim.yrs
0
1
11

CHOOSER
777
195
915
240
Species
Species
"N.melanderi" "E.pruinosa" "M.rotundata" "O.bicornis" "O.cornifrons" "O.cornuta" "O.lignaria"
3

SWITCH
426
318
546
351
MultiYearInput
MultiYearInput
1
1
-1000

INPUTBOX
426
411
512
471
Num.repeat.yr
2.0
1
0
Number

INPUTBOX
426
351
636
411
List.input.floral
NA
1
0
String

TEXTBOX
555
319
780
346
On: List.input.floral is used\nOff: Num.repeat.yr and input.floral are used
11
0.0
1

TEXTBOX
660
475
810
503
input.floral is repeated each simulated year
11
0.0
1

CHOOSER
779
285
871
330
Initial.stage
Initial.stage
"cocoon" "emerged" "nesting" "egg" "larva"
0

INPUTBOX
685
254
775
314
Initial.age
200.0
1
0
Number

INPUTBOX
593
628
690
688
emerged.survival
0.544
1
0
Number

INPUTBOX
595
254
685
314
Initial.num.m
200.0
1
0
Number

INPUTBOX
872
568
956
628
day.emerge.m
91.0
1
0
Number

INPUTBOX
872
627
956
687
var.emerge.m
2.0
1
0
Number

INPUTBOX
872
746
956
806
m.life
14.0
1
0
Number

INPUTBOX
689
568
785
628
max.survival.e.m
0.74
1
0
Number

TEXTBOX
446
541
583
569
Cell production parameters
11
0.0
1

TEXTBOX
654
540
726
568
Survival rates
11
0.0
1

TEXTBOX
811
539
933
557
Phenology and life spans\n
11
0.0
1

TEXTBOX
620
554
659
579
Female
11
0.0
1

TEXTBOX
723
553
751
571
Male
11
0.0
1

TEXTBOX
814
552
854
570
Female
11
0.0
1

TEXTBOX
904
552
928
570
Male
11
0.0
1

TEXTBOX
961
538
1029
566
Development \n(both sexes)
11
0.0
1

SWITCH
425
488
574
521
stoch.crop.forag
stoch.crop.forag
1
1
-1000

SWITCH
824
382
1003
415
Density.dep
Density.dep
1
1
-1000

INPUTBOX
824
473
911
533
DD.max.cells.s
2500.0
1
0
Number

INPUTBOX
824
415
911
475
DD.thresh.s
250.0
1
0
Number

CHOOSER
911
415
1003
460
DD.funct
DD.funct
"linear" "log"
0

INPUTBOX
929
459
1003
519
DD.log.slope
2.0
1
0
Number

TEXTBOX
825
335
1008
377
IMPORTANT: \nDD.thresh and DD.max.cells set in List.input.floral if MultiYearInput 'On'
11
0.0
1

TEXTBOX
18
881
1034
909
------------------------------------------------------------------------------------------------------------- Exposure Effect Module ----------------------------------------------------------------------------------------------------------------
11
0.0
1

SWITCH
426
901
529
934
Effects
Effects
0
1
-1000

SWITCH
529
901
695
934
Transfer.mat.adult
Transfer.mat.adult
0
1
-1000

SWITCH
555
1229
721
1262
Transfer.mat.dev
Transfer.mat.dev
0
1
-1000

INPUTBOX
425
1011
508
1071
ad.nectar.cons
208.34
1
0
Number

INPUTBOX
425
1071
508
1131
ad.pollen.cons
11.6
1
0
Number

INPUTBOX
555
1261
638
1321
SM
187.0
1
0
Number

INPUTBOX
553
1012
636
1072
ad.ET
0.072
1
0
Number

INPUTBOX
701
1262
784
1322
F
0.0
1
0
Number

INPUTBOX
553
1072
636
1132
TC_soil
0.5
1
0
Number

INPUTBOX
425
1131
508
1191
k_CA
0.4
1
0
Number

INPUTBOX
700
1011
784
1071
kd_SD
0.39
1
0
Number

INPUTBOX
700
1071
784
1131
mw_SD
13.0
1
0
Number

INPUTBOX
700
1131
784
1191
bw_SD
0.014
1
0
Number

INPUTBOX
844
1011
928
1071
kd_IT
0.012
1
0
Number

INPUTBOX
844
1071
928
1131
mw_IT
2.4
1
0
Number

INPUTBOX
844
1131
928
1191
Fs_IT
3.0
1
0
Number

TEXTBOX
434
974
621
1003
Adult exposure parameters\nNote dependence on nesting strategy
11
0.0
1

TEXTBOX
785
973
901
1001
Adult effect parameters 
11
0.0
1

TEXTBOX
721
992
768
1010
GUTS-SD
11
0.0
1

TEXTBOX
865
991
913
1009
GUTS-IT
11
0.0
1

INPUTBOX
428
1231
511
1291
nectar_prop
0.78
1
0
Number

INPUTBOX
701
1321
784
1381
SA_i
0.0
1
0
Number

INPUTBOX
844
1237
927
1297
dr.intercept
1.1534
1
0
Number

INPUTBOX
844
1296
927
1356
dr.slope
-0.7876
1
0
Number

SWITCH
529
934
695
967
Exposure.resting.soil
Exposure.resting.soil
1
1
-1000

TEXTBOX
435
1212
628
1240
In-nest stages exposure parameters
11
0.0
1

TEXTBOX
830
1206
964
1236
In-nest effect parameters \n(dose-response)
11
0.0
1

INPUTBOX
428
1291
510
1351
weight.prov
306.0
1
0
Number

CHOOSER
695
901
787
946
GUTS
GUTS
"GUTS-SD" "GUTS-IT" "NoEffects"
1

INPUTBOX
787
901
880
961
t.guts
10.0
1
0
Number

PLOT
10
895
381
1118
Deaths from exposure
Day
# deaths
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"in-nest" 1.0 0 -6459832 true "" "plot deaths.exp.in"
"adult" 1.0 0 -14730904 true "" "plot deaths.exp.ad"

INPUTBOX
553
1132
636
1192
TC_leaf
0.5
1
0
Number

TEXTBOX
512
1038
547
1056
mg/d
11
0.0
1

TEXTBOX
512
1096
547
1114
mg/d
11
0.0
1

TEXTBOX
639
1039
654
1063
d
11
0.0
1

TEXTBOX
639
1097
665
1115
g/d
11
0.0
1

TEXTBOX
639
1161
673
1179
cm2/d
11
0.0
1

TEXTBOX
885
928
903
946
1/d
11
0.0
1

TEXTBOX
788
1038
809
1056
1/d
11
0.0
1

TEXTBOX
787
1161
842
1179
bee*d/ng
11
0.0
1

TEXTBOX
788
1095
824
1113
ng/bee
11
0.0
1

TEXTBOX
932
1039
950
1057
1/d
11
0.0
1

TEXTBOX
933
1100
972
1118
ng/bee
11
0.0
1

TEXTBOX
515
1320
536
1338
mg
11
0.0
1

TEXTBOX
643
1290
671
1308
mg
11
0.0
1

TEXTBOX
793
1350
817
1368
cm2
11
0.0
1

CHOOSER
777
240
915
285
Voltinism
Voltinism
"univoltine" "multivoltine"
0

BUTTON
1032
197
1136
230
O. bicornis
set Start.day 1\nset Species \"O.bicornis\"\nset Voltinism \"univoltine\"\nset Initial.num.f 100\nset Initial.num.m 200\nset Initial.stage \"cocoon\"\nset Initial.age 200\nset RndSeed 42\nset MultiYearInput FALSE \nset List.input.floral \"NA\"\nset Num.repeat.yr 2\nset input.floral \"Floral_generic_optimal.csv\"\nset stoch.crop.forag FALSE\nset Density.dep FALSE\nset DD.thresh.s 250\nset DD.max.cells.s 2500\nset DD.funct \"linear\"\nset DD.log.slope 2\nset day.emerge.f 105\nset var.emerge.f 3\nset day.emerge.m 91\nset var.emerge.m 2\nset latest.emerge 365\nset dev.egg 8\nset dev.larva 32\nset dev.cocoon 68\nset t.maturation 3\nset m.life 14\nset max.nesting.life 36 \nset p.max.nesting.life 0.04\nset max.f.ratio 0.59\nset max.cells 2\nset max.survival.e.f 0.74\nset max.survival.e.m 0.74\nset emerged.survival 0.544\nset a.cell.age -0.006\nset a.sex.age -0.0406\nset a.size.age -0.003\nset a.cell.resource 0.94\nset a.sex.resource 0.42\nset a.size.resource 0.114\nset Effects FALSE\nset ad.nectar.cons 208.34\nset ad.pollen.cons 11.6\nset k_CA 0.4\nset Transfer.mat.adult TRUE\nset ad.ET 0.072\nset TC_soil 0.5\nset TC_leaf 0.5\nset Exposure.resting.soil FALSE\nset GUTS \"GUTS-IT\"\nset t.guts 10\nset kd_SD 0.39\nset bw_SD 0.014\nset mw_SD 13\nset kd_IT 0.012\nset mw_IT 2.4\nset Fs_IT 3\nset nectar_prop 0.78\nset weight.prov 306\nset Transfer.mat.dev TRUE\nset SM 187\nset F 0\nset SA_i 0\nset dr.intercept 1.1534\nset dr.slope -0.7876\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1032
230
1136
263
M. rotundata
set Start.day 1\nset Species \"M.rotundata\"\nset Voltinism \"univoltine\"\nset Initial.num.f 100\nset Initial.num.m 200\nset Initial.stage \"cocoon\"\nset Initial.age 200\nset RndSeed 42\nset MultiYearInput FALSE \nset List.input.floral \"NA\"\nset Num.repeat.yr 2\nset input.floral \"Floral_generic_optimal.csv\"\nset stoch.crop.forag FALSE\nset Density.dep FALSE\nset DD.thresh.s 250\nset DD.max.cells.s 2500\nset DD.funct \"linear\"\nset DD.log.slope 2\nset day.emerge.f 166\nset var.emerge.f 3\nset day.emerge.m 164\nset var.emerge.m 2\nset latest.emerge 185\nset dev.egg 3\nset dev.larva 9\nset dev.cocoon 8\nset t.maturation 7\nset m.life 7\nset max.nesting.life 52 \nset p.max.nesting.life 0.04\nset max.f.ratio 0.83\nset max.cells 2\nset max.survival.e.f 0.65\nset max.survival.e.m 0.65\nset emerged.survival 0.69\nset a.cell.age -0.006\nset a.sex.age -0.0406\nset a.size.age -0.003\nset a.cell.resource 0.94\nset a.sex.resource 0.42\nset a.size.resource 0.114\nset Effects FALSE\nset ad.nectar.cons 208.34\nset ad.pollen.cons 11.6\nset k_CA 0.4\nset Transfer.mat.adult TRUE\nset ad.ET 0.072\nset TC_soil 0.5\nset TC_leaf 0.5\nset Exposure.resting.soil FALSE\nset GUTS \"GUTS-IT\"\nset t.guts 10\nset kd_SD 0.39\nset bw_SD 0.014\nset mw_SD 13\nset kd_IT 0.012\nset mw_IT 2.4\nset Fs_IT 3\nset nectar_prop 0.78\nset weight.prov 306\nset Transfer.mat.dev TRUE\nset SM 187\nset F 0\nset SA_i 0\nset dr.intercept 1.1534\nset dr.slope -0.7876\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1032
263
1136
296
N. melanderi
set Start.day 1\nset Species \"N.melanderi\"\nset Voltinism \"univoltine\"\nset Initial.num.f 100\nset Initial.num.m 200\nset Initial.stage \"cocoon\"\nset Initial.age 200\nset RndSeed 42\nset MultiYearInput FALSE \nset List.input.floral \"NA\"\nset Num.repeat.yr 2\nset input.floral \"Floral_generic_optimal.csv\"\nset stoch.crop.forag FALSE\nset Density.dep FALSE\nset DD.thresh.s 250\nset DD.max.cells.s 2500\nset DD.funct \"linear\"\nset DD.log.slope 2\nset day.emerge.f 174\nset var.emerge.f 6\nset day.emerge.m 167\nset var.emerge.m 6\nset latest.emerge 365\nset dev.egg 2\nset dev.larva 6\nset dev.cocoon 20\nset t.maturation 1\nset m.life 14\nset max.nesting.life 26 \nset p.max.nesting.life 0.04\nset max.f.ratio 0.51\nset max.cells 1\nset max.survival.e.f 0.868\nset max.survival.e.m 0.868\nset emerged.survival 0.544\nset a.cell.age -0.006\nset a.sex.age -0.0406\nset a.size.age -0.003\nset a.cell.resource 0.94\nset a.sex.resource 0.42\nset a.size.resource 0.114\nset Effects FALSE\nset ad.nectar.cons 208.34\nset ad.pollen.cons 11.6\nset k_CA 0.4\nset Transfer.mat.adult TRUE\nset ad.ET 0.072\nset TC_soil 0.5\nset TC_leaf 0.5\nset Exposure.resting.soil FALSE\nset GUTS \"GUTS-IT\"\nset t.guts 10\nset kd_SD 0.39\nset bw_SD 0.014\nset mw_SD 13\nset kd_IT 0.012\nset mw_IT 2.4\nset Fs_IT 3\nset nectar_prop 0.78\nset weight.prov 306\nset Transfer.mat.dev TRUE\nset SM 187\nset F 0\nset SA_i 0\nset dr.intercept 1.1534\nset dr.slope -0.7876\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1032
296
1136
329
E. pruinosa
set Start.day 1\nset Species \"E.pruinosa\"\nset Voltinism \"univoltine\"\nset Initial.num.f 100\nset Initial.num.m 200\nset Initial.stage \"cocoon\"\nset Initial.age 200\nset RndSeed 42\nset MultiYearInput FALSE \nset List.input.floral \"NA\"\nset Num.repeat.yr 2\nset input.floral \"Floral_generic_optimal.csv\"\nset stoch.crop.forag FALSE\nset Density.dep FALSE\nset DD.thresh.s 250\nset DD.max.cells.s 2500\nset DD.funct \"linear\"\nset DD.log.slope 2\nset day.emerge.f 212\nset var.emerge.f 30\nset day.emerge.m 202\nset var.emerge.m 15\nset latest.emerge 365\nset dev.egg 8\nset dev.larva 15\nset dev.cocoon 68\nset t.maturation 4\nset m.life 14\nset max.nesting.life 36 \nset p.max.nesting.life 0.04\nset max.f.ratio 0.7\nset max.cells 1.65\nset max.survival.e.f 0.74\nset max.survival.e.m 0.74\nset emerged.survival 0.544\nset a.cell.age -0.006\nset a.sex.age -0.0406\nset a.size.age -0.003\nset a.cell.resource 0.94\nset a.sex.resource 0.42\nset a.size.resource 0.114\nset Effects FALSE\nset ad.nectar.cons 208.34\nset ad.pollen.cons 11.6\nset k_CA 0.4\nset Transfer.mat.adult TRUE\nset ad.ET 0.072\nset TC_soil 0.5\nset TC_leaf 0.5\nset Exposure.resting.soil FALSE\nset GUTS \"GUTS-IT\"\nset t.guts 10\nset kd_SD 0.39\nset bw_SD 0.014\nset mw_SD 13\nset kd_IT 0.012\nset mw_IT 2.4\nset Fs_IT 3\nset nectar_prop 0.78\nset weight.prov 306\nset Transfer.mat.dev TRUE\nset SM 187\nset F 0\nset SA_i 0\nset dr.intercept 1.1534\nset dr.slope -0.7876\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1036
105
1146
197
Default setup by species\nNote that Effect parameters are set to defaults for dimethoate
11
0.0
1

BUTTON
1031
350
1137
383
Version check
version.check
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
513
1157
536
1175
1/d
11
0.0
1

@#$#@#$#@
## SolBeePop_ecotox: A population model for pesticide risk assessments of solitary bees
SolBeePop_ecotox is an extension of SolBeePop (1), including exposures and effects to a pesticide.

(1) Schmolke, A., Galic, N., & Hinarejos, S. (2023). SolBeePop: A model of solitary bee populations in agricultural landscapes. Journal of Applied Ecology, 60, 2573–2585. https://doi.org/10.1111/1365-2664.14541

## Model description

A description of the model is provided in a separate document, following the ODD protocol and TRACE documenation. Model parameters (set on the interface), model procedures and state variables are described in a Table provided with the documentation.

In the code, additions for the ecotox module are marked with "Exposure Effect Module". The SolBeePop_ecototx code requires the extension "BeeGUTS.nls" in a separate file. A version check was added to the SolBeePop_ecotox code.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -1184463 true false 152 149 77 163 67 195 67 211 74 234 85 252 100 264 116 276 134 286 151 300 167 285 182 278 206 260 220 242 226 218 226 195 222 166
Polygon -16777216 true false 150 149 128 151 114 151 98 145 80 122 80 103 81 83 95 67 117 58 141 54 151 53 177 55 195 66 207 82 211 94 211 116 204 139 189 149 171 152
Polygon -7500403 true true 151 54 119 59 96 60 81 50 78 39 87 25 103 18 115 23 121 13 150 1 180 14 189 23 197 17 210 19 222 30 222 44 212 57 192 58
Polygon -16777216 true false 70 185 74 171 223 172 224 186
Polygon -16777216 true false 67 211 71 226 224 226 225 211 67 211
Polygon -16777216 true false 91 257 106 269 195 269 211 255
Line -1 false 144 100 70 87
Line -1 false 70 87 45 87
Line -1 false 45 86 26 97
Line -1 false 26 96 22 115
Line -1 false 22 115 25 130
Line -1 false 26 131 37 141
Line -1 false 37 141 55 144
Line -1 false 55 143 143 101
Line -1 false 141 100 227 138
Line -1 false 227 138 241 137
Line -1 false 241 137 249 129
Line -1 false 249 129 254 110
Line -1 false 253 108 248 97
Line -1 false 249 95 235 82
Line -1 false 235 82 144 100

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
