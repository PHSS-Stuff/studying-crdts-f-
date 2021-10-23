module StudyingCrdts.PNCounter

type PNCounter = GCounter.GCounter * GCounter.GCounter
let zero: PNCounter = (GCounter.zero,GCounter.zero)
let value (inc,dec) = GCounter.value inc - GCounter.value dec
let inc replica (inc,dec): PNCounter = (GCounter.inc replica inc, dec)
let dec replica (inc,dec): PNCounter = (inc, GCounter.inc replica dec)
let merge (inc1,dec1) (inc2,dec2) :PNCounter = (GCounter.merge inc1 inc2,GCounter.merge dec1 dec2)
