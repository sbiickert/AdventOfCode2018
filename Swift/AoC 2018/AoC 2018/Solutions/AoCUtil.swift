//
//  AoCUtil.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import Foundation
import OSLog

var solutions: [AoCSolution] {
	get {
		return [Day01(), Day02(), Day03(), Day04()]
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
	func solve(filename: String, index: Int) {
		print("Day \(String(format: "%02d", arguments: [day])): \(name) input: \(filename) [\(index)]");
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

class AoCUtil {
	public static let INPUT_FOLDER = "/Users/sjb/Developer/Advent of Code/2018/AdventOfCode2018/input"
	
	public static func fileName(day: Int, isTest: Bool) -> String {
		return "\(String(format: "%02d", arguments: [day])).\(isTest ? "test" : "challenge").txt"
	}
	
	public static func inputPath(for fileName: String) -> URL {
		let folderPath = URL(fileURLWithPath: INPUT_FOLDER, isDirectory: true)
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

extension StringProtocol {
	subscript(offset: Int) -> Character {
		self[index(startIndex, offsetBy: offset)]
	}
}
