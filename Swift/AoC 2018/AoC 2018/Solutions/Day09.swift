import Foundation
import Algorithms

class Day09: AoCSolution {
	override init() {
		super.init()
		day = 9
		name = "Marble Mania"
		emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		let pv = input[0].split(separator: ",")
		let playerCount = Int(pv[0])!
		let lastValue = Int(pv[1])!
		
		let result1 = solvePart(pCount: playerCount, lastValue: lastValue)
		
		return AoCResult(part1: String(result1), part2: nil)
	}
	
	private func solvePart(pCount: Int, lastValue: Int) -> Int {
		var currentMarbleValue = 0
		var currentPlayer = -1
		var scores = [Int](repeating: 0, count: pCount)
		
		let m = Marble(value: currentMarbleValue)
		m.insertBetween(ccwMarble: m, cwMarble: m)
		var currentMarble = m
		
		while currentMarbleValue < lastValue {
			currentPlayer += 1
			if currentPlayer >= pCount { currentPlayer = 0 }
			currentMarbleValue += 1
			
			if currentMarbleValue % 23 != 0 {
				// Insert a new marble clockwise, skipping one.
				currentMarble = currentMarble.cw!
				let ccw = currentMarble
				let cw = currentMarble.cw!
				currentMarble = Marble(value: currentMarbleValue)
				currentMarble.insertBetween(ccwMarble: ccw, cwMarble: cw)
				
			}
			else {
				// Keep the marble, take the one 7 spots ccw
				scores[currentPlayer] += currentMarbleValue
				for _ in 0..<7 { currentMarble = currentMarble.ccw! }
				let nextCurrentMarble = currentMarble.cw!
				currentMarble.remove()
				scores[currentPlayer] += currentMarble.value
				currentMarble = nextCurrentMarble
			}
		}
		
		let maxScore = scores.max()!
		print("The max score for pCount \(pCount) and lastValue \(lastValue) is \(maxScore)")
		
		return maxScore
	}
}

class Marble {
	static var all = Dictionary<Int, Marble>()
	
	let value: Int
	var cw: Marble?
	var ccw: Marble?
	
	init(value: Int) {
		self.value = value
		Marble.all[value] = self
	}

	func insertBetween(ccwMarble: Marble, cwMarble: Marble) {
		self.cw = ccwMarble.cw
		self.ccw = cwMarble.ccw
		ccwMarble.cw = self
		cwMarble.ccw = self
	}
	
	func remove() {
		ccw!.cw = self.cw
		cw!.ccw = self.ccw
	}
}
