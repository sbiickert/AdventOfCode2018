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
		let polymer = input[0]
		
		let result1 = solvePartOne(polymer)
		print("Part One: the length of the resulting polymer is \(result1.count).")
		
		let result2 = solvePartTwo(polymer)
		print("Part One: the length of the resulting polymer is \(result2.count).")
		
		return AoCResult(part1: String(result1.count), part2: String(result2.count))
	}
	
	private func solvePartOne(_ polymer: String) -> String {
		return reactRegex(polymer)
	}
	
	private let ALPHABET = "abcdefghijklmnopqrstuvwxyz"
	
	private func solvePartTwo(_ polymer: String) -> String {
		var bestUnit: String? = nil
		var bestPolymer: String? = nil
		let letters = ALPHABET.map( { String($0) } )

		for letter in letters {
			let filtered = polymer.replacingOccurrences(of: letter.uppercased(), with: "")
								  .replacingOccurrences(of: letter.lowercased(), with: "")
			let newPolymer = reactRegex(filtered)
			if bestPolymer == nil || newPolymer.count < bestPolymer!.count {
				bestPolymer = newPolymer
				bestUnit = letter
			}
		}
		print("The best unit to remove was \(bestUnit!).")
		
		return bestPolymer!
	}
	
	func reactRegex(_ polymer: String) -> String {
		let lc = ALPHABET.map( { String($0) } )
		let uc = ALPHABET.uppercased().map( { String($0) } )
		
		var reList = [NSRegularExpression]()
		for i in 0..<lc.count {
			let rE = try! NSRegularExpression(pattern: lc[i]+uc[i])
			reList.append(rE)
			let Re = try! NSRegularExpression(pattern: uc[i]+lc[i])
			reList.append(Re)
		}

		let pStr = NSMutableString(string: polymer)

		var len = 1000000
		while pStr.length != len {
			len = pStr.length
			for re in reList {
				re.replaceMatches(in: pStr, range: NSMakeRange(0, pStr.length), withTemplate: "")
			}
			//print(pStr.length)
		}
		
		let newPolymer = String(pStr)
		return newPolymer
	}
}
