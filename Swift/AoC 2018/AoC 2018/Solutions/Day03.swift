import Foundation
import Algorithms

class Day03: AoCSolution {
	override init() {
		super.init()
		day = 3
		name = "No Matter How You Slice It"
	}
	
	override func solve(filename: String, index: Int) {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let overlapCount = solvePartOne(input)
		print("Part One: the number of overlapping square inches is \(overlapCount)")
		
		let id = solvePartTwo(input)
		print("Part Two: the only non-overlapping claim is \(id)")
	}
	
	func solvePartOne(_ input: [String]) -> Int {
		var claims = [Claim]()
		for line in input {
			claims.append(Claim(def: line))
		}
		
		var overlaps = Set<String>()
		
		for combo in claims.combinations(ofCount: 2) {
			if combo[0].rect.intersects(combo[1].rect) {
				//print("\(combo[0]) intersects \(combo[1])")
				let intersection = combo[0].rect.intersection(combo[1].rect)
				for x in Int(intersection.minX)..<Int(intersection.maxX) {
					for y in Int(intersection.minY)..<Int(intersection.maxY) {
						overlaps.insert("\(x),\(y)")
					}
				}
			}
		}
		
		return overlaps.count
	}
	
	func solvePartTwo(_ input: [String]) -> Int {
		var ids = Set<Int>()
		var claims = [Claim]()
		
		for line in input {
			let c = Claim(def: line)
			claims.append(c)
			ids.insert(c.id)
		}
		
		for combo in claims.combinations(ofCount: 2) {
			if combo[0].rect.intersects(combo[1].rect) {
				ids.remove(combo[0].id)
				ids.remove(combo[1].id)
			}
		}
		
		return ids.first ?? -9999
	}
	
}

struct Claim: CustomDebugStringConvertible {
	var debugDescription: String {
		return "#\(id) @ \(rect.origin.x),\(rect.origin.y): \(rect.size.width)x\(rect.size.height)"
	}
	
	let id: Int
	let rect: CGRect
	
	init(def: String) {
		var stripped = def.replacingOccurrences(of: "#", with: "")
		stripped = stripped.replacingOccurrences(of: ":", with: "")
		let s = stripped.split(separator: " ", omittingEmptySubsequences: true)
		id = Int(s[0]) ?? -9999
		// s[1] is the @ sign
		let origin = s[2].split(separator: ",")
		let x = Int(origin[0]) ?? -9999
		let y = Int(origin[1]) ?? -9999
		let size = s[3].split(separator: "x")
		let w = Int(size[0]) ?? -9999
		let h = Int(size[1]) ?? -9999
		rect = CGRect(origin: CGPoint(x: x, y: y), size: CGSize(width: w, height: h))
	}
}
