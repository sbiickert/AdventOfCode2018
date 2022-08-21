//
//  Day08.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-21.
//

import Foundation

class Day08: AoCSolution {
	override init() {
		super.init()
		day = 8
		name = "Memory Maneuver"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let numbers = input.first!.split(separator: " ").compactMap { Int($0)}
		let root = Node.parseNodes(numbers)
		
		let result1 = String(root.metadataSum)
		print("Part One: the sum of the metadata is \(result1)")
		
		return AoCResult(part1: result1, part2: nil)

	}
}

private class Node {
	static func parseNodes(_ numbers: [Int]) -> Node {
		var n = numbers
		return parseNode(&n)
	}
	
	static func parseNode(_ numbers: inout [Int]) -> Node {
		let c = numbers.removeFirst()
		let m = numbers.removeFirst()
		let node = Node(c, m)
		for _ in 0..<c {
			node.children.append(parseNode(&numbers))
		}
		for _ in 0..<m {
			node.metadata.append(numbers.removeFirst())
		}
		return node
	}
		
	let childCount: Int
	let metadataCount: Int
	var children = [Node]()
	var metadata = [Int]()
	
	public init(_ c: Int, _ m: Int) {
		childCount = c
		metadataCount = m
	}
	
	var metadataSum: Int {
		var sum = metadata.reduce(0, +)
		for child in children {
			sum += child.metadataSum
		}
		return sum
	}
}
