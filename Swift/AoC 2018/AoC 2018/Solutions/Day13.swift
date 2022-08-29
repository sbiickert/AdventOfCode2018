import Foundation

class Day13: AoCSolution {
	override init() {
		super.init()
		day = 13
		name = "Mine Cart Madness"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		//let parsed = parseMap(input)
		
		return AoCResult(part1: nil, part2: nil)
	}
	
//	private func parseMap(_ input: [String]) -> (map: AoCGrid2D, carts: [MineCart]) {
//		//var map = AoCGrid2D(
//		
//	}
}

class MineCart {
	var position: AoCCoord2D
	var direction: AoCDirection
	private var lastTurn = TurnDir.none
	
	init(position pos: AoCCoord2D, direction dir: AoCDirection) {
		position = pos
		direction = dir
	}
	
	func move(on track: String) {
		if track == "\\" {
			if direction == .right || direction == .left	{	turnRight()	}
			else											{	turnLeft()	}
		}
		else if track == "/" {
			if direction == .up || direction == .down		{	turnRight()	}
			else											{	turnLeft()	}
		}
		else if track == "+" {
			chooseDirection()
		}
		moveForward()
	}
	
	func moveForward() {
		switch direction {
		case .up:
			position = AoCCoord2D(x: position.x, y: position.y-1)
		case .down:
			position = AoCCoord2D(x: position.x, y: position.y+1)
		case .right:
			position = AoCCoord2D(x: position.x+1, y: position.y)
		case .left:
			position = AoCCoord2D(x: position.x-1, y: position.y)
		}
	}
	
	func chooseDirection() {
		switch lastTurn {
		case .none:
			turnRight()
		case .left:
			lastTurn = .none
		case .right:
			turnLeft()
		}
	}
	
	func turnLeft() {
		switch direction {
		case .up:
			direction = .left
		case .down:
			direction = .right
		case .right:
			direction = .up
		case .left:
			direction = .down
		}
		lastTurn = .left
	}
	
	func turnRight() {
		switch direction {
		case .up:
			direction = .right
		case .down:
			direction = .left
		case .right:
			direction = .down
		case .left:
			direction = .up
		}
		lastTurn = .right
	}
	
	enum TurnDir {
		case none
		case left
		case right
	}
}
