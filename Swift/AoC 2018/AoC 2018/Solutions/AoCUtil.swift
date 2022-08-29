//
//  AoCUtil.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import Foundation
import OSLog
import Algorithms

var solutions: [AoCSolution] {
	get {
		return [Day01(), Day02(), Day03(), Day04(), Day05(),
				Day06(), Day07(), Day08(), Day09(), Day10(),
				Day11(), Day12()]
	}
}

func inputs(for solution: AoCSolution) -> [AoCInput] {
	var inputs = [AoCInput]()
	// Challenge input. N = 1
	inputs.append(AoCInput(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: false), index: 0))
	
	// Test input. N = 0..N
	if solution.emptyLinesIndicateMultipleInputs {
		let testGroups = AoCUtil.readGroupedInputFile(
			named: AoCUtil.fileName(day: solution.day, isTest: true))
		for (i, _) in testGroups.enumerated() {
			inputs.append(AoCInput(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: true), index: i))
		}
	}
	else {
		inputs.append(AoCInput.init(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: true), index: 0))
	}
	
	return inputs
}

class AoCSolution {
	var day: Int = 0
	var name: String = ""
	var emptyLinesIndicateMultipleInputs: Bool = true
	@discardableResult func solve(filename: String, index: Int) -> AoCResult {
		print("Day \(String(format: "%02d", arguments: [day])): \(name) input: \(filename) [\(index)]");
		return AoCResult(part1: "", part2: "")
	}
}

struct AoCInput {
	let solution: AoCSolution
	let fileName: String
	let index: Int
	var id: String {
		return "\(fileName)[\(index)]"
	}
}

struct AoCResult {
	let part1: String?
	let part2: String?
}

class AoCUtil {
	public static let INPUT_FOLDER_MIKE = "/Users/sjb/Developer/Advent of Code/2018/AdventOfCode2018/input"
	public static let INPUT_FOLDER_CLARIS = "/Users/sbiickert/Code/Advent of Code/2018/AdventOfCode2018/input"
	public static let ALPHABET = "abcdefghijklmnopqrstuvwxyz"

	public static func fileName(day: Int, isTest: Bool) -> String {
		return "\(String(format: "%02d", arguments: [day])).\(isTest ? "test" : "challenge").txt"
	}
	
	public static func inputPath(for fileName: String) -> URL {
		var isDir: ObjCBool = true
		var folderPath = URL(fileURLWithPath: INPUT_FOLDER_MIKE, isDirectory: true)
		if FileManager.default.fileExists(atPath: folderPath.path, isDirectory: &isDir) == false {
			folderPath = URL(fileURLWithPath: INPUT_FOLDER_CLARIS, isDirectory: true)
		}
		let filePath = folderPath.appendingPathComponent(fileName)
		//print(filePath)
		return filePath
	}
	
	public static func readInputFile(named name:String, removingEmptyLines removeEmpty:Bool) -> [String] {
		var results = [String]()
		do {
			let path = inputPath(for: name)
			let data = try Data(contentsOf: path)
			if let input = String(data: data, encoding: .utf8) {
				results = input.components(separatedBy: "\n")
			}
			else {
				os_log("Could not decode \(path) as UTF-8")
			}
		} catch {
			os_log("Could not read file \(name)")
		}
		if removeEmpty {
			results = results.filter { $0.count > 0 }
		}
		return results
	}
	
	public static func readGroupedInputFile(named name: String, group: Int) -> [String] {
		let result = [String]()
		guard group >= 0 else { return result }
		
		let groups = readGroupedInputFile(named: name)
		guard group < groups.count else { return result }
		
		return groups[group]
	}
	
	public static func readGroupedInputFile(named name: String) -> [[String]] {
		var results = [[String]]()
		let lines = readInputFile(named: name, removingEmptyLines: false)
		
		var group = [String]()
		for line in lines {
			if line.count > 0 {
				group.append(line)
			}
			else {
				results.append(group)
				group = [String]()
			}
		}
		if group.count > 0 {
			results.append(group)
		}
		
		return results
	}
	
	static func rangeToArray(r: Range<Int>) -> [Int] {
		var result = [Int]()
		for i in r {
			result.append(i)
		}
		return result
	}
	
	static func cRangeToArray(r: ClosedRange<Int>) -> [Int] {
		var result = [Int]()
		for i in r {
			result.append(i)
		}
		return result
	}
}

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

class AoCGrid2D {
	var width: Int
	var height: Int
	let defaultValue: String
	var _data = Dictionary<AoCCoord2D, String>()
	var neighbourRule: NeighbourRule = .rook
	
	enum NeighbourRule {
		case rook
		case bishop
		case queen
	}
	
	init(width: Int, height: Int, defaultValue: String = ".") {
		self.width = width > 0 ? width : 1
		self.height = height > 0 ? height : 1
		self.defaultValue = defaultValue
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
		for (row, col) in product(0...height, 0...width) {
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
		for row in 0...height {
			var values = [String]()
			for col in 0...width {
				values.append(value(at: AoCCoord2D(x: col, y: row)))
			}
			print(values.joined(separator: ""))
		}
		print("")
	}
}

/* MARK: Extensions */

extension String {
	subscript(offset: Int) -> Character {
		get {
			self[index(startIndex, offsetBy: offset)]
		}
		set {
			let idx = self.index(startIndex, offsetBy: offset)
			self.replaceSubrange(idx...idx, with: [newValue])
		}
	}
}

extension String {
	func indicesOf(string: String) -> [Int] {
		var indices = [Int]()
		var searchStartIndex = self.startIndex

		while searchStartIndex < self.endIndex,
			let range = self.range(of: string, range: searchStartIndex..<self.endIndex),
			!range.isEmpty
		{
			let d = distance(from: self.startIndex, to: range.lowerBound)
			indices.append(d)
			searchStartIndex = self.index(after: self.index(startIndex, offsetBy: d))
		}

		return indices
	}
}

extension NSRegularExpression {
	convenience init(_ pattern: String) {
		do {
			try self.init(pattern: pattern)
		} catch {
			preconditionFailure("Illegal regular expression: \(pattern).")
		}
	}

	func matches(_ string: String) -> Bool {
		let range = NSRange(location: 0, length: string.utf16.count)
		return firstMatch(in: string, options: [], range: range) != nil
	}

	func positionalMatches(_ string: String) -> [String] {
		var result = [String]()
		let range = NSRange(location: 0, length: string.utf16.count)
		if let match = firstMatch(in: string, range: range) {
			for i in 1..<match.numberOfRanges {
				let r = match.range(at: i)
				let low = string.index(string.startIndex, offsetBy: r.lowerBound)
				let hi = string.index(string.startIndex, offsetBy: r.upperBound)
				result.append(String(string[low..<hi]))
			}
		}
		return result
	}
	
	func allMatches(_ string: String) -> [String] {
		var result = [String]()
		var searchRange = NSRange(location: 0, length: string.utf16.count)
		var matchRange = self.rangeOfFirstMatch(in: string, range: searchRange)
		
		while (matchRange.length > 0) {
			let low = string.index(string.startIndex, offsetBy: matchRange.lowerBound)
			let hi = string.index(string.startIndex, offsetBy: matchRange.upperBound)
			result.append(String(string[low..<hi]))
			searchRange = NSRange(location: matchRange.upperBound, length: searchRange.length - matchRange.upperBound)
			if searchRange.length <= 0 { break }
			matchRange = self.rangeOfFirstMatch(in: string, range: searchRange)
		}
		
		return result
	}
}
