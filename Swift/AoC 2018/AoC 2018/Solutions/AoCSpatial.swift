//
//  AoCSpatial.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation
import Algorithms

struct AoCCoord2D: Hashable {
	let x: Int
	let y: Int
	
	static func +(left: AoCCoord2D, right: AoCCoord2D) -> AoCCoord2D {
		return AoCCoord2D(x: left.x + right.x, y: left.y + right.y)
	}
	
	static func -(left: AoCCoord2D, right: AoCCoord2D) -> AoCCoord2D {
		return AoCCoord2D(x: left.x - right.x, y: left.y - right.y)
	}
	
	static func readingOrderSort(c0: AoCCoord2D, c1: AoCCoord2D) -> Bool {
		if c0.y == c1.y {
			return c0.x < c1.x
		}
		return c0.y < c1.y
	}

	func manhattanDistance(to other: AoCCoord2D) -> Int {
		return abs(self.x - other.x) + abs(self.y - other.y)
	}
	
	func isAdjacent(to other: AoCCoord2D, rule: AoCAdjacencyRule = .rook) -> Bool {
		switch rule {
		case .rook:
			return self.manhattanDistance(to: other) == 1
		case .bishop:
			return abs(x - other.x) == 1 && abs(y - other.y) == 1
		case .queen:
			return (self.manhattanDistance(to: other) == 1) || (abs(x - other.x) == 1 && abs(y - other.y) == 1)
		}
	}
	
	func getAdjacent(rule: AoCAdjacencyRule = .rook) -> [AoCCoord2D] {
		var result = [AoCCoord2D]()
		for offset in getAdjacentOffsets(rule: rule) {
			result.append(self + offset)
		}
		return result
	}
	
	func getAdjacentOffsets(rule: AoCAdjacencyRule = .rook) -> [AoCCoord2D] {
		switch rule {
		case .rook:
			return [AoCCoord2D(x: -1, y:  0), AoCCoord2D(x:  1, y:  0),
					AoCCoord2D(x:  0, y: -1), AoCCoord2D(x:  0, y:  1)]
		case .bishop:
			return [AoCCoord2D(x: -1, y: -1), AoCCoord2D(x:  1, y:  1),
					AoCCoord2D(x:  1, y: -1), AoCCoord2D(x: -1, y:  1)]
		case .queen:
			return [AoCCoord2D(x: -1, y:  0), AoCCoord2D(x:  1, y:  0),
					AoCCoord2D(x:  0, y: -1), AoCCoord2D(x:  0, y:  1),
					AoCCoord2D(x: -1, y: -1), AoCCoord2D(x:  1, y:  1),
					AoCCoord2D(x:  1, y: -1), AoCCoord2D(x: -1, y:  1)]
		}
	}
	
	var description: String {
		return "[x: \(x), y: \(y)]"
	}
}

struct AoCExtent2D: Hashable {
	static func build(from coords: [AoCCoord2D]) -> AoCExtent2D {
		if let (xmin, xmax) = (coords.map { $0.x }).minAndMax(),
		   let (ymin, ymax) = (coords.map { $0.y }).minAndMax() {
			return AoCExtent2D(min: AoCCoord2D(x: xmin, y: ymin), max: AoCCoord2D(x: xmax, y: ymax))
		}
		return AoCExtent2D(min: AoCCoord2D(x: 0, y: 0), max: AoCCoord2D(x: 0, y: 0))
	}
	
	let min: AoCCoord2D
	let max: AoCCoord2D
	
	var width: Int {
		return max.x - min.x + 1
	}
	
	var height: Int {
		return max.y - min.y + 1
	}
	
	func contains(_ coord: AoCCoord2D) -> Bool {
		return min.x <= coord.x && coord.x <= max.x &&
			   min.y <= coord.y && coord.y <= max.y
	}
}

enum AoCDirection: String {
	case up = "^"
	case down = "v"
	case right = ">"
	case left = "<"
}

enum AoCAdjacencyRule {
	case rook
	case bishop
	case queen
}

class AoCGrid2D {
	let defaultValue: String
	var _data = Dictionary<AoCCoord2D, String>()
	var neighbourRule: AoCAdjacencyRule = .rook
	
	init(defaultValue: String = ".") {
		self.defaultValue = defaultValue
	}
	
	var extent: AoCExtent2D {
		return AoCExtent2D.build(from: [AoCCoord2D](_data.keys))
	}

	func value(at coord: AoCCoord2D) -> String {
		if let v = _data[coord] {
			return v
		}
		return defaultValue
	}
	
	func setValue(_ v: String, at coord: AoCCoord2D) {
		_data[coord] = v
	}
	
	var coords: [AoCCoord2D] {
		return Array(_data.keys)
	}
	
	var counts: Dictionary<String, Int> {
		var result = Dictionary<String, Int>()
		let ext = extent
		for (row, col) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let v = value(at: AoCCoord2D(x: col, y: row))
			if result.keys.contains(v) == false { result[v] = 0 }
			result[v]! += 1
		}
		return result
	}
	
	func neighbourOffsets(at coord: AoCCoord2D) -> [AoCCoord2D] {
		return coord.getAdjacentOffsets(rule: self.neighbourRule)
	}
	
	func neighbourCoords(at coord: AoCCoord2D) -> [AoCCoord2D] {
		return coord.getAdjacent(rule: self.neighbourRule)
	}
	
	func neighbourCoords(at coord: AoCCoord2D, withValue s: String) -> [AoCCoord2D] {
		var result = neighbourCoords(at: coord)
		result = result.filter { self.value(at: $0) == s }
		return result
	}

	func draw() {
		let ext = extent
		for row in ext.min.y...ext.max.y {
			var values = [String]()
			for col in ext.min.x...ext.max.x {
				values.append(value(at: AoCCoord2D(x: col, y: row)))
			}
			print(values.joined(separator: " "))
		}
		print("")
	}
	
	func findLeastCostPath(from source: AoCCoord2D, to target: AoCCoord2D,
						   barrierValue: String,
						   additionalBarriers: Set<AoCCoord2D>?) -> (cost: Int, path: [AoCCoord2D]) {
		// Assuming constant cost for now
		struct Node {
			var cost = 1
			var minTravelCost = Int.max
			var visited = false
			var isBarrier = false
		}
		// extent might not have origin at 0,0
		// so there are corrections below to translate the grid
		let ext = extent
		var grid = [[Node]](repeating: [Node](repeating: Node(), count: ext.width), count: ext.height)
		// Mark all barriers
		for (r, c) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let coord = AoCCoord2D(x: c, y: r)
			if value(at: coord) == barrierValue || (additionalBarriers != nil && additionalBarriers!.contains(coord)) {
				grid[r - ext.min.y][c - ext.min.x].isBarrier = true
			}
		}
		
		var pos = AoCCoord2D(x: source.x - ext.min.x, y: source.y - ext.min.y)
		grid[pos.y][pos.x].minTravelCost = 0
		
		while grid[target.y - ext.min.y][target.x - ext.min.x].visited == false {
			let neighbours = pos.getAdjacent()
			for n in neighbours {
				if ext.contains(n) && grid[n.y][n.x].visited == false && !grid[n.y][n.x].isBarrier {
					let costToN = grid[pos.y][pos.x].minTravelCost + grid[n.y][n.x].cost
					if grid[n.y][n.x].minTravelCost > costToN { grid[n.y][n.x].minTravelCost = costToN }
				}
			}
			grid[pos.y][pos.x].visited = true
			
			var minCost = Int.max
			var minCostPos: AoCCoord2D?
			for (r,c) in product(0..<ext.height, 0..<ext.width) {
				if grid[r][c].visited == false && grid[r][c].minTravelCost < minCost {
					minCost = grid[r][c].minTravelCost
					minCostPos = AoCCoord2D(x: c, y: r)
				}
			}
			
			// If the lowest cost node has minTravelCost Int.Max,
			// then there was no path from source to target
			if minCost == Int.max { break }
			
			pos = minCostPos!
		}
		
		for r in grid {
			print((r.map { String($0.minTravelCost == Int.max ? 0 : $0.minTravelCost) }).joined(separator: " "))
		}

		return (cost: -1, path: [AoCCoord2D]())
	}
}
