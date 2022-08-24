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

		return AoCResult(part1: result.c.description, part2: nil)
	}
	
	private func findMaxPower(_ grid: [[Int]], size: Int) -> (p: Int, c: AoCCoord2D, g: [[Int]]) {
		var maxCoord: AoCCoord2D?
		var maxPower = Int.min
		var outGrid = [[Int]](repeating: [Int](repeating: 0, count: Day11.GRID_SIZE-(size-1)), count: Day11.GRID_SIZE-(size-1))
		
		for (r, c) in product(0...(Day11.GRID_SIZE-size), 0...(Day11.GRID_SIZE-size)) {
			var sumPower = 0
			
			for (x, y) in product(0..<size, 0..<size) {
				sumPower += grid[r+y][c+x]
			}
			
			if sumPower > maxPower {
				outGrid[r][c] = sumPower
				maxPower = sumPower
				maxCoord = AoCCoord2D(x: c, y: r)
			}
		}
		
		return (p: maxPower, c: maxCoord!, g: outGrid)
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
