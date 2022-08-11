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
		
		let result1 = solvePartOne(polymer)
		print("Part One: the length of the resulting polymer is \(result1.stringValue.count).")
		
		let result2 = solvePartTwo(polymer)
		print("Part One: the length of the resulting polymer is \(result2.stringValue.count).")
		
		return AoCResult(part1: String(result1.stringValue.count), part2: String(result2.stringValue.count))
	}
	
	private func solvePartOne(_ polymer: SuitPolymer) -> SuitPolymer {
//		return reactWindowed(polymer)
		return reactRegex(polymer)
	}
	
	private func solvePartTwo(_ polymer: SuitPolymer) -> SuitPolymer {
		var bestUnit: String? = nil
		var bestPolymer: SuitPolymer? = nil
		let letters = "abcdefghijklmnopqrstuvwxyz".map( { String($0) } )

		for letter in letters {
			let newPolymer = reactRegex(polymer, collapseUnitRegardlessOfPolarity: letter)
			if bestPolymer == nil || newPolymer.units.count < bestPolymer!.units.count {
				bestPolymer = newPolymer
				bestUnit = letter
			}
		}
		print("The best unit to remove was \(bestUnit!).")
		
		return bestPolymer!
	}
	
	func reactRegex(_ polymer: SuitPolymer, collapseUnitRegardlessOfPolarity: String? = nil) -> SuitPolymer {
		let lc = "abcdefghijklmnopqrstuvwxyz".map( { String($0) } )
		let uc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".map( { String($0) } )
		
		var reList = [NSRegularExpression]()
		for i in 0..<lc.count {
			let rE = try! NSRegularExpression(pattern: lc[i]+uc[i])
			reList.append(rE)
			let Re = try! NSRegularExpression(pattern: uc[i]+lc[i])
			reList.append(Re)
		}

		var pStr = NSMutableString(string: polymer.stringValue)
		
		if let unit = collapseUnitRegardlessOfPolarity {
			pStr = NSMutableString(string: pStr.replacingOccurrences(of: unit.lowercased(), with: ""))
			pStr = NSMutableString(string: pStr.replacingOccurrences(of: unit.uppercased(), with: ""))
		}

		var len = 1000000
		while pStr.length != len {
			len = pStr.length
			for re in reList {
				re.replaceMatches(in: pStr, range: NSMakeRange(0, pStr.length), withTemplate: "")
			}
			//print(pStr.length)
		}
		
		let str = String(pStr)
		let newPolymer = SuitPolymer(units: str.map( { String($0) } ))
		return newPolymer
	}

	func reactWindowed(_ polymer: SuitPolymer) -> SuitPolymer {
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
