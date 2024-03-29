import Foundation
import Algorithms

class Day06: AoCSolution {
	override init() {
		super.init()
		day = 6
		name = "Chronal Coordinates"
	}

	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)

		let result1 = solvePartOne(input)
		print("Part One: the largest non-infinite area is \(result1)")
		
		let limit = (input.count < 20) ? 32 : 10000
		let result2 = solvePartTwo(input, limit: limit)
		print("Part Two: the area with total distance less than \(limit) is \(result2)")

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePartOne(_ input: [String]) -> String {
		let grid = parseGrid(input)
		//grid.draw()
		allocateGrid(grid)
		//grid.draw()
		var counts = grid.counts
		//print("\(counts as AnyObject)")
		let edgeValues = getInfiniteValues(grid)
		for v in edgeValues {
			counts.removeValue(forKey: v)
		}
		//print("\(counts as AnyObject)")
		let result:String = String(counts.values.sorted().last!)
		
		return result
	}

	private func solvePartTwo(_ input: [String], limit: Int) -> String {
		let grid = parseGrid(input)
		var count = 0
		let coords = grid.coords
		let ext = grid.extent
		
		for (row, col) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let target = AoCCoord2D(x: col, y: row)
			var sumDistance = 0
			for coord in coords {
				sumDistance += target.manhattanDistance(to: coord)
			}
			if sumDistance < limit { count += 1 }
		}
		return String(count)
	}

	private func getInfiniteValues(_ grid: AoCGrid2D) -> [String] {
		var values = Set<String>()
		let ext = grid.extent
		for (row, col) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			if row == ext.min.y || row == ext.max.y || col == ext.min.x || col == ext.max.x {
				values.insert(grid.value(at: AoCCoord2D(x: col, y: row)))
			}
		}
		return Array(values)
	}
		
	private func allocateGrid(_ grid: AoCGrid2D) {
		let coords = grid.coords
		let ext = grid.extent
		for (row, col) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let target = AoCCoord2D(x: col, y: row)
			var minValue = ""
			var minMD = ext.height + ext.width + 10 // Bigger MD than can be contained by grid
			
			for coord in coords {
				let md = coord.manhattanDistance(to: target)
				if md < minMD {
					minValue = grid.value(at: coord)
					minMD = md
				}
				else if md == minMD {
					// Tie
					minValue += grid.value(at: coord)
					minMD = md
				}
			}
			
			if minValue.count == 1 {
				grid.setValue(minValue, at: target)
			}
		}
	}
	
	private func parseGrid(_ input: [String]) -> AoCGrid2D {
		var coords = [AoCCoord2D]()
		for var line in input {
			line = line.replacingOccurrences(of: " ", with: "")
			let xy = line.split(separator: ",")
			let x = Int(xy[0])!
			let y = Int(xy[1])!
			coords.append(AoCCoord2D(x: x, y: y))
		}
		
		let grid = AoCGrid2D()
		
		let UC_LC_ALPHABET = AoCUtil.ALPHABET.uppercased() + AoCUtil.ALPHABET.lowercased()
		for i in 0..<coords.count {
			grid.setValue(String(UC_LC_ALPHABET[i]), at: coords[i])
		}
		
		return grid
	}
}
