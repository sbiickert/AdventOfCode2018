import Foundation

class Day04: AoCSolution {
	override init() {
		super.init()
		day = 4
		name = "Repose Record"
	}

	override func solve(filename: String, index: Int) {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let events = parseEvents(input)
		
		let result = solvePartOne(events)
		print("Part One: the guard id * the minutes = \(result)")
	}
	
	func solvePartOne(_ events: [GuardEvent]) -> Int {
		var sleepRecord = Dictionary<Int, Int>()
		
		for (i, event) in events.enumerated() {
			if event.type == .fallAsleep {
				let wakeEvent = events[i+1]
				let diffComponents = Calendar.current.dateComponents([.minute], from: event.time, to: wakeEvent.time)
				let minutes = diffComponents.minute
				if sleepRecord.keys.contains(event.guardID!) == false {
					sleepRecord[event.guardID!] = 0
				}
				sleepRecord[event.guardID!]! += minutes ?? 0
			}
		}

		print(sleepRecord as AnyObject)

		var maxSleep = 0
		var sleepiestGuard = events[0].guardID!
		for guardID in sleepRecord.keys {
			if sleepRecord[guardID]! > maxSleep {
				maxSleep = sleepRecord[guardID]!
				sleepiestGuard = guardID
			}
		}
		
		print("The sleepiest guard is guard #\(sleepiestGuard) with \(maxSleep) minutes")
		
		return 0
	}
	
	func parseEvents(_ input: [String]) -> [GuardEvent] {
		var events = [GuardEvent]()
		for line in input {
			let event = GuardEvent(def: line)
			events.append(event)
		}
		
		// Events may be out of order
		events.sort { $0.time < $1.time }
		
		// Assign the guard id
		var guardID = events[0].guardID!
		for (i, event) in events.enumerated() {
			if event.guardID != nil {
				guardID = event.guardID!
			}
			else {
				events[i].guardID = guardID
			}
		}
		return events
	}
}

struct GuardEvent {
	static let REGEX = "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] (.+)"
	static let GUARD_REGEX = "Guard #(\\d+)"
	let time: Date
	let type: GuardEventType
	var guardID: Int?
	
	init(def: String) {
		let regex = NSRegularExpression(GuardEvent.REGEX)
		if regex.matches(def) {
			let captures = regex.positionalMatches(def)
			var dc = DateComponents()
			dc.year = 2018 //Int(captures[0]) 1518 breaks foundation. Year doesn't matter.
			dc.month = Int(captures[1])
			dc.day = Int(captures[2])
			dc.hour = Int(captures[3])
			dc.minute = Int(captures[4])
			time = Calendar.current.date(from: dc)!
			if captures[5].hasSuffix("asleep") {
				type = .fallAsleep
			}
			else if captures[5].hasSuffix("up") {
				type = .wake
			}
			else {
				type = .start
				let gregex = NSRegularExpression(GuardEvent.GUARD_REGEX)
				guardID = Int(gregex.positionalMatches(captures[5])[0])
			}
		}
		else {
			time = Date()
			type = .err
		}
	}
}

enum GuardEventType {
	case start
	case wake
	case fallAsleep
	case err
}
