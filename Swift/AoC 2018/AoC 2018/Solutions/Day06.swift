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
		let grid = parseGrid(input)
		//grid.draw()
		allocateGrid(grid)
		//grid.draw()
		var counts = grid.counts
		print("\(counts as AnyObject)")
		let edgeValues = getInfiniteValues(grid)
		for v in edgeValues {
			counts.removeValue(forKey: v)
		}
		print("\(counts as AnyObject)")
		let result1:String = String(counts.values.sorted().last!)
		
		print("Part One: the largest non-infinite area is \(result1)")

		return AoCResult(part1: result1, part2: nil)
	}
	
	private func getInfiniteValues(_ grid: AoCGrid2D) -> [String] {
		var values = Set<String>()
		for (row, col) in product(0...grid.height, 0...grid.width) {
			if row == 0 || row == grid.height || col == 0 || col == grid.width {
				values.insert(grid.value(at: AoCCoord2D(x: col, y: row)))
			}
		}
		return Array(values)
	}
		
	private func allocateGrid(_ grid: AoCGrid2D) {
		let coords = grid.coords
		
		for (row, col) in product(0...grid.height, 0...grid.width) {
			let target = AoCCoord2D(x: col, y: row)
			var minValue = ""
			var minMD = grid.height + grid.width + 10 // Bigger MD than can be contained by grid
			
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
//			else {
//				print("\(target) is equidistant (\(minMD)) from \(minValue)")
//			}
		}
	}
	
	private func parseGrid(_ input: [String]) -> AoCGrid2D {
		var coords = [AoCCoord2D]()
		var xmax = 0
		var ymax = 0
		for var line in input {
			line = line.replacingOccurrences(of: " ", with: "")
			let xy = line.split(separator: ",")
			let x = Int(xy[0])!
			let y = Int(xy[1])!
			coords.append(AoCCoord2D(x: x, y: y))
			xmax = max(xmax, x)
			ymax = max(ymax, y)
		}
		
		let grid = AoCGrid2D(width: xmax, height: ymax)
		
		let ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		for i in 0..<coords.count {
			grid.setValue(String(ALPHABET[i]), at: coords[i])
		}
		
		return grid
	}
}
