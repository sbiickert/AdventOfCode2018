//
//  Day15.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation

class Day15: AoCSolution {
	override init() {
		super.init()
		name = "Beverage Bandits"
		day = 15
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		let parsed = parseInput(input)
		
		let result1 = solvePartOne(parsed)
		print("Part One: the outcome score is \(result1)")
		
		return AoCResult(part1: String(result1), part2: nil)
	}
	
	private func solvePartOne(_ input: ParseResult) -> Int {
		var round = 0
		var people = input.startPeople
		var map = input.map
		
		while (people.filter {$0.kind == .elf}).count > 0
				&& (people.filter {$0.kind == .goblin}).count > 0 {
			playRound(map: map, people: &people)
			round += 1
			break
		}
		
		let score = calcScore(numberOfRounds: round, people: people)
		
		if let check = input.endPeople {
			let checkScore = calcScore(numberOfRounds: round, people: check)
			if score != checkScore {
				print("The score (\(score)) is not the same as predicted (\(checkScore)")
			}
		}
		return score
	}
	
	private func playRound(map: AoCGrid2D, people: inout [Combatant]) {
		var killed = Set<Combatant>()
		
		// Sort people into reading order (top to bottom, left to right)
		people.sort(by: Combatant.readingOrderSort(c0:c1:))
		
		for person in people {
			if killed.contains(person) { continue }
			let alivePeople = people.filter { !killed.contains($0) }
			
			// Find targets
			let allTargets = alivePeople.filter { $0.kind != person.kind }
			
			// Get open squares next to targets
			var coordsInRangeOfTargets = Set<AoCCoord2D>()
			for target in allTargets {
				map.neighbourCoords(at: target.position, withValue: ".").forEach {
					coordsInRangeOfTargets.insert($0)
				}
			}
			// Minus open squares that are occupied by people other than person
			let otherPeopleCoords = Set((alivePeople.filter { $0 != person }).map { $0.position })
			coordsInRangeOfTargets.subtract(otherPeopleCoords)
			
			if coordsInRangeOfTargets.isEmpty { continue } // End turn
			
			if coordsInRangeOfTargets.contains(person.position) == false {
				// Person is not next to a target. Have to move.
				let _ = map.findLeastCostPath(from: person.position, to: coordsInRangeOfTargets.first!,
												 barrierValue: "#", additionalBarriers: otherPeopleCoords)
			}
			
			if coordsInRangeOfTargets.contains(person.position) == true {
				// Attack the weakest target in range
				var targetsInRange = allTargets.filter { $0.position.isAdjacent(to: person.position) }
				targetsInRange.sort { $0.hp < $1.hp }
				let weakestTarget = targetsInRange.first!
				weakestTarget.hp -= Combatant.ATTACK_POWER
				if weakestTarget.hp <= 0 {
					killed.insert(weakestTarget)
				}
			}
		}
		
		people.removeAll { killed.contains($0) }
	}
	
	private func calcScore(numberOfRounds: Int, people: [Combatant]) -> Int {
		let sum = (people.map { $0.hp }).reduce(0, +)
		return sum * numberOfRounds
	}
	
	private struct ParseResult {
		let map: AoCGrid2D
		let startPeople: [Combatant]
		var endPeople: [Combatant]?
	}
	
	private func parseInput(_ input: [String]) -> ParseResult {
		let groupSplittingRegex = NSRegularExpression("(\\S+)")
		let hpRegex = NSRegularExpression("(\\d+)")
		var map = AoCGrid2D()
		var people = [[Combatant]]()
		let speciesStrings = Combatant.Species.allCases.map { $0.rawValue }
		
		for (row, line) in input.enumerated() {
			let groups = groupSplittingRegex.allMatches(line)
			var endStatePeopleAddedInThisRow = [Combatant]()
			
			for g in 0..<groups.count {
				if g == 0 || g == 1 {
					// 0 is start situation, 1 is end situation
					if g == people.count { people.append([Combatant]()) }
					for (col, ch) in Array(groups[g]).enumerated() {
						var s = String(ch)
						let coord = AoCCoord2D(x: col, y: row)
						if speciesStrings.contains(s) {
							let person = Combatant(Combatant.Species.init(rawValue: s)!, position: coord)
							people[g].append(person)
							if g == 1 { endStatePeopleAddedInThisRow.append(person) }
							s = "."
						}
						map.setValue(s, at: coord) // Redundant when g = 1 but who cares
					}
				}
				else {
					// all info about Combatants and expected score
					if groups[g].starts(with: "Outcome") { break } // ignoring outcome for now
					// This is either E(000) or G(000). Need the number for the hp
					let person = endStatePeopleAddedInThisRow.removeFirst()
					person.hp = Int(hpRegex.positionalMatches(groups[g]).first!)!
				}
			}
		}
		
		var result = ParseResult(map: map, startPeople: people[0])
		map.draw()
		if people.count > 1 {
			result.endPeople = people[1]
		}
		return result
	}
}

private class Combatant: Hashable {
	static let ATTACK_POWER = 3
	
	static func == (lhs: Combatant, rhs: Combatant) -> Bool {
		return lhs.id == rhs.id
	}
	
	static func readingOrderSort(c0: Combatant, c1: Combatant) -> Bool {
		return AoCCoord2D.readingOrderSort(c0: c0.position, c1: c1.position)
	}

	enum Species: String, CaseIterable {
		case elf = "E"
		case goblin = "G"
	}
	
	let id = UUID()
	let kind: Species
	var position: AoCCoord2D
	var hp: Int = 200
	
	init(_ species: Species, position pos: AoCCoord2D) {
		kind = species
		position = pos
	}
	
	func hash(into hasher: inout Hasher) {
		hasher.combine(id.hashValue)
	}
	
}
