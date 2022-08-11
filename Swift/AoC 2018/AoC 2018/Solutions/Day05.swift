import Foundation
import Algorithms

class Day05: AoCSolution {
	override init() {
		super.init()
		day = 5
		name = "Alchemical Reduction"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let polymer = SuitPolymer(units: input[0].map( { String($0) } ))
		
		let result = solvePartOne(polymer)
		print("Part One: the length of the resulting polymer is \(result.stringValue.count).")
		
		return AoCResult(part1: String(result.stringValue.count), part2: nil)
	}
	
	private func solvePartOne(_ polymer: SuitPolymer) -> SuitPolymer {
		var newPolymer = polymer
		while newPolymer.units.count > 2 {
			//print(newPolymer.stringValue)
			let windowed = newPolymer.units.windows(ofCount: 2)
			var resultUnits = [String]()
			var bLastWindowReacted = false
			for w in windowed {
				if bLastWindowReacted {
					bLastWindowReacted = false
					continue
				}
				//print(w, w.startIndex)
				let a = w[w.startIndex]
				let b = w[w.startIndex+1]
				if a != b && a.lowercased() == b.lowercased() {
					//print("boom")
					bLastWindowReacted = true
				}
				else {
					resultUnits.append(a)
					bLastWindowReacted = false
				}
			}
			if bLastWindowReacted == false { resultUnits.append(polymer.units.last!) }

			if resultUnits.count != newPolymer.units.count {
				newPolymer = SuitPolymer(units: resultUnits)
				print(resultUnits.count)
			}
			else {
				break
			}
		}
		return newPolymer
	}
}

struct SuitPolymer {
	var units: [String]
	
	var stringValue: String {
		return units.joined(separator: "")
	}
}
