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

	func manhattanDistance(to other: AoCCoord2D) -> Int {
		return abs(self.x - other.x) + abs(self.y - other.y)
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
		return max.x - min.x
	}
	
	var height: Int {
		return max.y - min.y
	}
}

enum AoCDirection: String {
	case up = "^"
	case down = "v"
	case right = ">"
	case left = "<"
}

class AoCGrid2D {
	let defaultValue: String
	var _data = Dictionary<AoCCoord2D, String>()
	var neighbourRule: NeighbourRule = .rook
	
	enum NeighbourRule {
		case rook
		case bishop
		case queen
	}
	
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
		var result = [AoCCoord2D]()
		switch neighbourRule {
		case .rook, .queen:
			result.append(AoCCoord2D(x: -1, y:  0))
			result.append(AoCCoord2D(x:  1, y:  0))
			result.append(AoCCoord2D(x:  0, y: -1))
			result.append(AoCCoord2D(x:  0, y:  1))
		case .bishop, .queen:
			result.append(AoCCoord2D(x: -1, y: -1))
			result.append(AoCCoord2D(x:  1, y:  1))
			result.append(AoCCoord2D(x:  1, y: -1))
			result.append(AoCCoord2D(x: -1, y:  1))
		}
		return result
	}
	
	func neighbourCoords(at coord: AoCCoord2D) -> [AoCCoord2D] {
		var result = [AoCCoord2D]()
		for offset in neighbourOffsets(at: coord) {
			result.append(coord + offset)
		}
		return result
	}
	
	func draw() {
		let ext = extent
		for row in ext.min.y...ext.max.y {
			var values = [String]()
			for col in ext.min.x...ext.max.x {
				values.append(value(at: AoCCoord2D(x: col, y: row)))
			}
			print(values.joined(separator: ""))
		}
		print("")
	}
}

