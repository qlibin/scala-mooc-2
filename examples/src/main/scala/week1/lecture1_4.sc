Some("value").map(s => s + "+addition")
Some("value").flatMap(s => Some("value"))


Some(1).flatMap(i=>Some(2)).flatMap(j=>Some(3))
Some(1).flatMap(i=>Some(2).flatMap(j=>Some(3)))