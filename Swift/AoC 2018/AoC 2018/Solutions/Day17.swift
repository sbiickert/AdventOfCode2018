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
		var cursor = Cursor(state: .falling, position: wellPosition)
		
		while !(cursor.state == .falling && cursor.position.y > ext.max.y) {
			switch cursor.state {
			case .falling:
				let nextPos = cursor.position.coord(offsetByX: 0, y: 1)
				let content = scan.value(at: nextPos)
				if content == "#" || content == "~" {
					// Blocked. Start flooding
					
				}
				else {
					scan.setValue("|", at: nextPos)
					cursor.position = nextPos
				}
			}
		}
		
		let sum = scan.getCoords(withValue: "~").count + scan.getCoords(withValue: "|").count
		
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

enum CursorState {
	case falling
	case flooded
	case flowingLeft
	case flowingRight
}

struct Cursor {
	var state: CursorState
	var position: AoCCoord2D
}
