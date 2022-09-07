//
//  Day17.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-09-06.
//

import Foundation

class Day17: AoCSolution {
	override init() {
		super.init()
		name = "Reservoir Research"
		day = 17
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let scan = parseScan(input)
		
		let result1 = solvePartOne(scan)
		print("Part One: the number of squares filled with water is \(result1)")
		
		return AoCResult(part1: String(result1), part2: nil)
	}
	
	private func solvePartOne(_ scan: AoCGrid2D) -> Int {
		let ext = scan.extent
		let wellPosition = scan.getCoords(withValue: "+").first!
		var cursor = wellPosition.coord(offsetByX: 0, y: 1)
		
		var backtrackPositions = [AoCCoord2D]()
		
		while cursor.y <= ext.max.y || backtrackPositions.count > 0 {
			if cursor.y > ext.max.y { cursor = backtrackPositions.removeFirst() }
			
			let below = HydroState(rawValue: scan.value(at: cursor.coord(offsetByX: 0, y: 1)))
			
			switch below {
			case .empty:
				scan.setValue(HydroState.falling.rawValue, at: cursor)
				cursor = cursor.coord(offsetByX: 0, y: 1)
			case .barrier, .flooded:
				// Blocked. Start flooding
				var leftPos = cursor.coord(offsetByX: -1, y: 0)
				var leftVal = scan.value(at: leftPos)
				var leftBelowVal = scan.value(at: leftPos.coord(offsetByX: 0, y: 1))
				while leftVal != HydroState.barrier.rawValue &&
						(leftBelowVal == HydroState.barrier.rawValue || leftBelowVal == HydroState.flooded.rawValue) {
					leftPos = leftPos.coord(offsetByX: -1, y: 0)
					leftVal = scan.value(at: leftPos)
					leftBelowVal = scan.value(at: leftPos.coord(offsetByX: 0, y: 1))
				}
				var rightPos = cursor.coord(offsetByX: 1, y: 0)
				var rightVal = scan.value(at: rightPos)
				var rightBelowVal = scan.value(at: rightPos.coord(offsetByX: 0, y: 1))
				while rightVal != HydroState.barrier.rawValue &&
						(rightBelowVal == HydroState.barrier.rawValue || rightBelowVal == HydroState.flooded.rawValue) {
					rightPos = rightPos.coord(offsetByX: 1, y: 0)
					rightVal = scan.value(at: rightPos)
					rightBelowVal = scan.value(at: rightPos.coord(offsetByX: 0, y: 1))
				}
				if leftVal == HydroState.barrier.rawValue && rightVal == HydroState.barrier.rawValue {
					// Flood row and move up
					for x in leftPos.x+1..<rightPos.x {
						scan.setValue(HydroState.flooded.rawValue, at: AoCCoord2D(x: x, y: cursor.y))
					}
					cursor = cursor.coord(offsetByX: 0, y: -1)
				}
				else if leftVal == HydroState.barrier.rawValue && rightBelowVal == HydroState.empty.rawValue {
					// Flowing right and move to right
					for x in leftPos.x+1..<rightPos.x {
						scan.setValue(HydroState.flowingRight.rawValue, at: AoCCoord2D(x: x, y: cursor.y))
					}
					cursor = rightPos
				}
				else if leftBelowVal == HydroState.empty.rawValue && rightVal == HydroState.barrier.rawValue {
					// Flowing left and move to left
					for x in leftPos.x+1..<rightPos.x {
						scan.setValue(HydroState.flowingLeft.rawValue, at: AoCCoord2D(x: x, y: cursor.y))
					}
					cursor = leftPos
				}
				else if leftBelowVal == HydroState.empty.rawValue && rightBelowVal == HydroState.empty.rawValue {
					// Flowing left and right
					for x in leftPos.x+1..<rightPos.x {
						scan.setValue(HydroState.falling.rawValue, at: AoCCoord2D(x: x, y: cursor.y))
					}
					cursor = leftPos
					backtrackPositions.append(rightPos)
				}
				else {
					print("leftVal \(leftVal), leftBelowVal \(leftBelowVal), rightVal \(rightVal), rightBelowVal \(rightBelowVal), shouldn't happen.")
					var m = Dictionary<AoCCoord2D, String>()
					m[cursor] = "X"
					for bt in backtrackPositions { m[bt] = "O" }
					scan.draw(markers: m)				}
			default:
				print("below is \(below) and that shouldn't happen")
			}
		}
		scan.draw()
		
		let sum = scan.getCoords(withValue: HydroState.flooded.rawValue).count
			+ scan.getCoords(withValue: HydroState.falling.rawValue).count
			+ scan.getCoords(withValue: HydroState.flowingLeft.rawValue).count
			+ scan.getCoords(withValue: HydroState.flowingRight.rawValue).count

		return sum
	}
	
	
	
	private func parseScan(_ input: [String]) -> AoCGrid2D {
		let scan = AoCGrid2D()
		scan.setValue("+", at: AoCCoord2D(x: 500, y: 0))
		
		let regex = NSRegularExpression("([xy])=(\\d+), ([xy])=(\\d+)\\.\\.(\\d+)")
		
		for line in input {
			let matches = regex.positionalMatches(line)
			let range = ClosedRange<Int>(uncheckedBounds: (lower: Int(matches[3])!, upper: Int(matches[4])!))
			for run in range {
				var x = Int(matches[1])!
				var y = run
				if matches[0] == "y" {
					x = run
					y = Int(matches[1])!
				}
				let coord = AoCCoord2D(x: x, y: y)
				scan.setValue("#", at: coord)
			}
		}
		
		//scan.draw()
		return scan
	}
}

enum HydroState: String {
	case falling = "|"
	case flooded = "~"
	case flowingLeft = "<"
	case flowingRight = ">"
	case barrier = "#"
	case empty = "."
}

