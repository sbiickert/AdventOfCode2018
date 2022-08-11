import Foundation

class Day06: AoCSolution {
	override init() {
		super.init()
		day = 6
		name = "Chronal Coordinates"
	}

	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		return AoCResult(part1: nil, part2: nil)
	}
}
