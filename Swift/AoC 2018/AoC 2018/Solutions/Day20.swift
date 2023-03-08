//
//  Day20.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-09-12.
//

import Foundation

class Day20: AoCSolution {
	override init() {
		super.init()
		day = 20
		name = "A Regular Map"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		var input = AoCUtil.readGroupedInputFile(named: filename, group: index).first!
		input.removeFirst()
		input.removeLast()
		//print(input)
		
		let chars = Array(input).map { String($0) }
		let map = makeMap(chars)
		
		let (part1, part2) = solvePart(map: map)
		
		return AoCResult(part1: String(part1), part2: String(part2))
	}
	
	private func solvePart(map: AoCGrid2D) -> (Int, Int) {
		let start = map.getCoords(withValue: "X")[0]
		
		var doorCount = 0
		var isDoor = true
		var visited = Set<AoCCoord2D>();
		var visitedBeyondThousand = 0 // 17011 too high, 8507 too low
		visited.insert(start);
		var toVisit = Set<AoCCoord2D>(map.neighbourCoords(at: start, withValue: "D"))
		while (true) {
			var nextSteps = Set<AoCCoord2D>();
			for step in toVisit {
				let neighbors = map.neighbourCoords(at: step, withValue: isDoor ? "." : "D")
				for n in neighbors {
					if !visited.contains(n) {
						nextSteps.insert(n)
					}
				}
				visited.insert(step)
				if !isDoor && doorCount >= 1000 {
					visitedBeyondThousand += 1
				}
			}
			if isDoor {
				doorCount += 1
			}
			isDoor = !isDoor
			
			toVisit = nextSteps
			if toVisit.count == 0 {
				break
			}
		}
		//let roomCount = map.getCoords(withValue: ".").count
		return (doorCount, visitedBeyondThousand)
	}
	
	private func makeMap(_ input: [String]) -> AoCGrid2D {
		let pos = AoCCoord2D(x: 10000, y: 10000)
		let map = AoCGrid2D(defaultValue: "#")
		map.setValue("X", at: pos)
				
		mapTree(map, tree: input[0..<input.count], from: pos)
		
		let ext = map.extent
		map.setValue("#", at: ext.min.coord(offsetByX: -1, y: -1))
		map.setValue("#", at: ext.max.coord(offsetByX: 1, y: 1))

		//map.draw()
		
		return map
	}
	
	private func mapTree(_ map: AoCGrid2D, tree: ArraySlice<String>, from pos: AoCCoord2D) {
		//print("mapTree " + tree.joined())
		var pos = pos
		var start = tree.startIndex
		var end = start
		while start < tree.endIndex {
			if tree[start] == "(" {
				var count = 1
				var pipes = [Int]()
				while count > 0 {
					if tree[end] == "(" { count += 1 }
					if tree[end] == ")" { count -= 1 }
					if count == 1 && tree[end] == "|" { pipes.append(end) }
					end += 1
				}
				start += 1
				for pipe in pipes {
					mapTree(map, tree: tree[start..<pipe], from: pos)
					start = pipe + 1
				}
				mapTree(map, tree: tree[start..<end-1], from: pos)
				start = end
			}
			while end < tree.endIndex && tree[end] != "(" { end += 1 }
			if start < end {
				pos = mapSimpleBranch(map, branch: tree[start..<end], from: pos)
			}
			start = end
			end += 1
		}
	}
	
	private func mapSimpleBranch(_ map: AoCGrid2D, branch: ArraySlice<String>, from pos: AoCCoord2D) -> AoCCoord2D {
		//print("mapSimpleBranch " + branch.joined())
		var pos = pos
		for index in branch.startIndex..<branch.endIndex {
			let letter = String(branch[index])
			if letter == "$" { break }
			var dx: Int = 0
			var dy: Int = 0
			switch AoCMapDirection(rawValue: letter)! {
				case .north:
					dy = -1
				case .south:
					dy = 1
				case .east:
					dx = 1
				case .west:
					dx = -1
			}
			let doorPos = pos.coord(offsetByX: dx, y: dy)
			map.setValue("D", at: doorPos)
			let roomPos = doorPos.coord(offsetByX: dx, y: dy)
			map.setValue(".", at: roomPos)
			
			pos = roomPos
		}
		return pos
	}
}
