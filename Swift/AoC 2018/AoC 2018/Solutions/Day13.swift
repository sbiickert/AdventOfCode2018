import Foundation
import Algorithms
import SwiftUI

class Day13: AoCSolution {
	override init() {
		super.init()
		day = 13
		name = "Mine Cart Madness"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		
		var parsed = parseMap(input)
		let result1 = solvePartOne(map: parsed.map, carts: parsed.carts)
		print("Part One: the location of the first crash is \(result1)")
		
		parsed = parseMap(input) // Re-parse. Part One will have changed the data
		let result2 = solvePartTwo(map: parsed.map, carts: parsed.carts)
		print("Part Two: the location of the last cart is \(result2)")

		return AoCResult(part1: result1.description, part2: result2.description)
	}
	
	private func solvePartOne(map: AoCGrid2D, carts: [MineCart]) -> AoCCoord2D {
		var carts = carts
		var collisionPosition: AoCCoord2D? = nil
		
		draw(map: map, carts: carts, mark: collisionPosition)

		while collisionPosition == nil {
			// Sort carts by position row, col
			carts = carts.sorted {
				$0.position.y == $1.position.y ?
				$0.position.x < $1.position.x :
				$0.position.y < $1.position.y
			}
			
			for cart in carts {
				let positions = Set<AoCCoord2D>(carts.map {$0.position})
				cart.move(on: map.value(at: cart.position))
				if positions.contains(cart.position) {
					collisionPosition = cart.position
					break
				}
			}
			draw(map: map, carts: carts, mark: collisionPosition)
		}
		
		return collisionPosition!
	}
	
	private func solvePartTwo(map: AoCGrid2D, carts: [MineCart]) -> AoCCoord2D {
		var carts = carts

		draw(map: map, carts: carts)

		while carts.count > 1 {
			// Sort carts by position row, col
			carts = carts.sorted {
				$0.position.y == $1.position.y ?
				$0.position.x < $1.position.x :
				$0.position.y < $1.position.y
			}
			
			var eliminated = Set<MineCart>()
			var cartPositions = Dictionary<AoCCoord2D, MineCart>()
			carts.forEach {cartPositions[$0.position] = $0}
			
			for cart in carts {
				if eliminated.contains(cart) {continue}
				
				let oldPos = cart.position
				cart.move(on: map.value(at: cart.position))
				
				if cartPositions.keys.contains(cart.position) {
					print("A collision at \(cart.position).")
					eliminated.insert(cart)
					eliminated.insert(cartPositions[cart.position]!)
					cartPositions.removeValue(forKey: oldPos)
					cartPositions.removeValue(forKey: cart.position)
				}
				else {
					cartPositions.removeValue(forKey: oldPos)
					cartPositions[cart.position] = cart
				}
			}
			
			carts = carts.filter { !eliminated.contains($0) }
			
			draw(map: map, carts: carts)
		}
		
		return carts.first!.position
	}
	
	private func draw(map: AoCGrid2D, carts: [MineCart], mark: AoCCoord2D? = nil) {
		let ext = map.extent
		if ext.width > 50 { return }
		var cartPositions = Dictionary<AoCCoord2D, MineCart>()
		carts.forEach {cartPositions[$0.position] = $0}
		
		for row in ext.min.y...ext.max.y {
			var values = [String]()
			for col in ext.min.x...ext.max.x {
				let pos = AoCCoord2D(x: col, y: row)
				if mark != nil && mark! == pos {
					values.append("X")
				}
				else if cartPositions.keys.contains(pos) {
					values.append(cartPositions[pos]!.direction.rawValue)
				}
				else {
					values.append(map.value(at: pos))
				}
			}
			print(values.joined(separator: ""))
		}
		print()
	}

	private func parseMap(_ input: [String]) -> (map: AoCGrid2D, carts: [MineCart]) {
		let  map = AoCGrid2D(defaultValue: " ")
		var carts = [MineCart]()
		let TRACK_PARTS: Set = ["+", "|", "-", "/", "\\"]
		
		for (r, c) in product(0..<input.count, 0..<input[0].count) {
			let char = String(input[r][c])
			if TRACK_PARTS.contains(char) {
				map.setValue(char, at: AoCCoord2D(x: c, y: r))
			}
			else {
				// A mine cart
				if let dir = AoCDirection(rawValue: char) {
					let cart = MineCart(position: AoCCoord2D(x: c, y: r), direction: dir)
					carts.append(cart)
					switch dir {
						case .up, .down:
							map.setValue("|", at: AoCCoord2D(x: c, y: r))
						case .left, .right:
							map.setValue("-", at: AoCCoord2D(x: c, y: r))
					}
				}
			}
		}
		
		return (map: map, carts: carts)
	}
}

class MineCart: Hashable {
	static func == (lhs: MineCart, rhs: MineCart) -> Bool {
		return lhs.id == rhs.id
	}
	
	func hash(into hasher: inout Hasher) {
		id.hash(into: &hasher)
	}
	
	let id = UUID()
	var position: AoCCoord2D
	var direction: AoCDirection
	private var lastTurnChosen = TurnDir.right // So that the first turn will be to the left
	
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
		switch lastTurnChosen {
		case .none:
			turnRight()
			lastTurnChosen = .right
		case .left:
			lastTurnChosen = .none
		case .right:
			turnLeft()
			lastTurnChosen = .left
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
	}
	
	enum TurnDir {
		case none
		case left
		case right
	}
}
