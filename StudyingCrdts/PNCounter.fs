module StudyingCrdts.PNCounter

type PNCounter = GCounter.GCounter * GCounter.GCounter
let zero: PNCounter = (GCounter.zero, GCounter.zero)
let value (inc,dec) =  GCounter.value inc - GCounter.value dec
let inc r (inc,dec):PNCounter = (GCounter.inc r inc , dec)
let dec r (inc,dec):PNCounter = (inc,GCounter.inc r dec)
let merge (inc1,dec1) (inc2,dec2) : PNCounter = (GCounter.merge inc1 inc2, GCounter.merge dec1 dec2)
