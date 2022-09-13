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
		print(input)
		
		var chars = Array(input).map { String($0) }
		let map = makeMap(chars)
		
		return AoCResult(part1: input, part2: nil)
	}
	
	private func makeMap(_ input: [String]) -> AoCGrid2D {
		var pos = AoCCoord2D(x: 10000, y: 10000)
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
			var door = "|"
			switch AoCMapDirection(rawValue: letter)! {
				case .north:
					dy = -1
					door = "-"
				case .south:
					dy = 1
					door = "-"
				case .east:
					dx = 1
				case .west:
					dx = -1
			}
			let doorPos = pos.coord(offsetByX: dx, y: dy)
			map.setValue(door, at: doorPos)
			let roomPos = doorPos.coord(offsetByX: dx, y: dy)
			map.setValue(".", at: roomPos)
			
			pos = roomPos
		}
		return pos
	}
}
