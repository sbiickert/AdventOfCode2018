import Foundation
import Algorithms

class Day02: AoCSolution {
	override init() {
		super.init()
		day = 2
		name = "Inventory Management System"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let checksum = solvePartOne(input)
		
		print("Part One: the checksum is \(checksum)")
		
		let commonLetters = solvePartTwo(input)
		print("Part Two: the common letters are \(commonLetters)")
		
		return AoCResult(part1: String(checksum), part2: commonLetters)
	}
	
	private func solvePartOne(_ input: [String]) -> Int {
		var countWithExactlyTwo = 0
		var countWithExactlyThree = 0
		
		for line in input {
			var counts = Dictionary<Character, Int>()
			for char in line {
				if counts.keys.contains(char) == false {
					counts[char] = 0
				}
				counts[char]! += 1
			}

			var hasTwo = false
			var hasThree = false
			for char in counts.keys {
				if counts[char] == 2 {hasTwo = true}
				if counts[char] == 3 {hasThree = true}
			}
			
			if hasTwo { countWithExactlyTwo += 1 }
			if hasThree { countWithExactlyThree += 1 }
		}
		
		return countWithExactlyTwo * countWithExactlyThree
	}
	
	private func solvePartTwo(_ input: [String]) -> String {
		for combo in input.combinations(ofCount: 2) {
			var common = ""
			for i in 0..<combo[0].count {
				if combo[0][i] == combo[1][i] {
					common += String(combo[0][i])
				}
			}
			if common.count == combo[0].count - 1 {
				return common
			}
		}
		return "Error"
	}
}
