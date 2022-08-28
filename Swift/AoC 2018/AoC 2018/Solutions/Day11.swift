import Foundation
import Algorithms

class Day11: AoCSolution {
	static let GRID_SIZE = 300
	
	override init() {
		super.init()
		day = 11
		name = "Chronal Charge"
	}

	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let serialNumber = Int(input.first!)!
		
		let grid = buildGrid(serialNumber: serialNumber)
		let result = findMaxPower(grid, size: 3)
		
		print("Part One: the coord with the maximum power is \(result.c.description) with power \(result.p)")
		
		let result2 = solvePartTwo(grid)

		return AoCResult(part1: result.c.description, part2: result2)
	}
	
	private func solvePartTwo(_ grid: [[Int]]) -> String {
		var grids = Dictionary<Int, [[Int]]>()
		grids[1] = grid
		
		var maxPower = Int.min
		var maxCoord: AoCCoord2D?
		var maxSize = 0
		for size in 1...Day11.GRID_SIZE {
			print(size)
			let f = Day11.largestFactor(size)
			let r = findMaxPower(grids[f]!, size: size, factor: f)
			
			grids[size] = r.g

			if r.p > maxPower {
				maxPower = r.p
				maxSize = size
				maxCoord = r.c
				print("Power \(maxPower) -> \(maxCoord!.description) @ \(maxSize)")
			}
		}

		print("Part Two: the coord with the maximum power is \(maxCoord!.description) with power \(maxPower) at size \(maxSize)")

		return "\(maxCoord!.description) @ \(maxSize)"
	}
	
	private func findMaxPower(_ grid: [[Int]], size: Int, factor: Int = 1) -> (p: Int, c: AoCCoord2D, g: [[Int]]) {
		var maxCoord: AoCCoord2D?
		var maxPower = Int.min
		var outGrid = [[Int]](repeating: [Int](repeating: 0, count: Day11.GRID_SIZE-(size-1)),
							  count: Day11.GRID_SIZE-(size-1))
		
		for (r, c) in product(0...(Day11.GRID_SIZE-size), 0...(Day11.GRID_SIZE-size)) {
			var sumPower = 0
			
			for x in stride(from: 0, to: size, by: factor) {
				for y in stride(from: 0, to: size, by: factor) {
					sumPower += grid[r+y][c+x]
				}
			}
			
			outGrid[r][c] = sumPower

			if sumPower > maxPower {
				maxPower = sumPower
				maxCoord = AoCCoord2D(x: c, y: r)
			}
		}
		
		return (p: maxPower, c: maxCoord!, g: outGrid)
	}
	
	private static func largestFactor(_ number: Int) -> Int {
		for f in stride(from: number-1, to: 0, by: -1) {
			if (number % f == 0) {
				return f
			}
		}
		return 1
	}
	
	private func buildGrid(serialNumber: Int) -> [[Int]] {
		var grid = [[FuelCell]]()
		for r in 0..<Day11.GRID_SIZE {
			var row = [FuelCell]()
			for c in 0..<Day11.GRID_SIZE {
				row.append(FuelCell(x: c, y: r))
			}
			grid.append(row)
		}
		let pGrid = grid.map { $0.map { $0.realPowerLevel(serialNumber: serialNumber) }}
		return pGrid
	}
	
	private func runPowerTests() {
//		Fuel cell at  3,5, grid serial number 8: power level 4.
		var cell = FuelCell(x: 3, y: 5)
		print(cell.realPowerLevel(serialNumber: 8))
//		Fuel cell at  122,79, grid serial number 57: power level -5.
		cell = FuelCell(x: 122, y: 79)
		print(cell.realPowerLevel(serialNumber: 57))
//		Fuel cell at 217,196, grid serial number 39: power level  0.
		cell = FuelCell(x: 217, y: 196)
		print(cell.realPowerLevel(serialNumber: 39))
//		Fuel cell at 101,153, grid serial number 71: power level  4.
		cell = FuelCell(x: 101, y: 153)
		print(cell.realPowerLevel(serialNumber: 71))
	}
}

private struct FuelCell {
	let x: Int
	let y: Int
	
	var rackID: Int {
		return x + 10
	}
	
	var basePowerLevel: Int {
		return rackID * y
	}
	
	func realPowerLevel(serialNumber: Int) -> Int {
		let increased = (basePowerLevel + serialNumber) * rackID
		let hundreds = FuelCell.hundredsDigit(number: increased)
		return hundreds - 5
	}
	
	private static func hundredsDigit(number: Int) -> Int {
		let h:Int = number / 100
		return h % 10;
	}
}
