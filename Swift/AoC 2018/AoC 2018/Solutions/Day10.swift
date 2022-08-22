//
//  Day10.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-21.
//

import Foundation

class Day10: AoCSolution {
	override init() {
		super.init()
		day = 10
		name = "The Stars Align"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		var stars = [Star]()
		for line in input {
			stars.append(Star.parse(from: line))
		}
		
		var t = 0
		var size = Int.max
		var newSize = Int.max - 1
		var xmin = Int.max
		var ymin = Int.max
		var xmax = Int.min
		var ymax = Int.min

		repeat {
			size = newSize
			xmin = Int.max
			ymin = Int.max
			xmax = Int.min
			ymax = Int.min
			for s in 0..<stars.count {
				stars[s].move()
				xmin = min(xmin, stars[s].location.x)
				ymin = min(ymin, stars[s].location.y)
				xmax = max(xmax, stars[s].location.x)
				ymax = max(ymax, stars[s].location.y)
			}
			newSize = (xmax - xmin) * (ymax - ymin)
			t += 1
		} while newSize < size
		
		var lookup = Dictionary<AoCCoord2D, Star>()
		for s in 0..<stars.count {
			stars[s].move(forward: false)
			lookup[stars[s].location] = stars[s]
		}

		for r in ymin...ymax {
			var row = [String]()
			for c in xmin...xmax {
				row.append(lookup.keys.contains(AoCCoord2D(x: c, y: r)) ? "#" : ".")
			}
			print(row.joined())
		}
		
		
		return AoCResult(part1: "See output for printed letters", part2: String(t-1))
	}
}

struct Star {
	static func parse(from input:String) -> Star {
		let regex = NSRegularExpression("< ?([-\\d]+), +([-\\d]+)> velocity=< ?([-\\d]+), +([-\\d]+)>")
		let matches = regex.positionalMatches(input)
		let loc = AoCCoord2D(x: Int(matches[0])!, y: Int(matches[1])!)
		let dr = AoCCoord2D(x: Int(matches[2])!, y: Int(matches[3])!)
		return Star(location: loc, drift: dr)
	}
	
	var location: AoCCoord2D
	let drift: AoCCoord2D
	
	mutating func move(forward: Bool = true) {
		if forward {
			self.location = self.location + self.drift
		}
		else {
			self.location = self.location - self.drift
		}
	}
}
