import Foundation

class Day01: AoCSolution {
	override init() {
		super.init()
		day = 1
		name = "Chronal Calibration"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: false)
		
		let frequency = solvePartOne(input)
		print("Part One: the frequency is \(frequency)")
		
		let repeatedFrequency = solvePartTwo(input)
		print("Part Two: the first repeated frequency is \(repeatedFrequency)")
		
		return AoCResult(part1: String(frequency), part2: String(repeatedFrequency))
	}
	
	private func solvePartOne(_ input: [String]) -> Int {
		var frequency = 0
		for line in input {
			if let i = Int(line) {
				frequency += i
			}
		}
		return frequency
	}
	
	private func solvePartTwo(_ input: [String]) -> Int {
		var frequency = 0
		var record = Set<Int>()
		record.insert(frequency)
		
		while (true) {
			for line in input {
				if let i = Int(line) {
					frequency += i
					if record.contains(frequency) {
						return frequency
					}
					record.insert(frequency)
				}
			}
		}
		//return -9999
	}
}
