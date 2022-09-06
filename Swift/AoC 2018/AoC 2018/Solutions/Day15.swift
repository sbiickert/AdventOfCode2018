//
//  Day15.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation
import Algorithms

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
		Combatant.elfAttackPowerBoost = 0
		
		let result1 = solvePartOne(parsed)
		print("Part One: the outcome score is \(result1)")
		
		let result2 = solvePartTwo(parsed)
		print("Part Two: the outcome score with no elf deaths is \(result2)")
		
		return AoCResult(part1: String(result1), part2: String(result2))
	}
	
	private func solvePartOne(_ input: ParseResult) -> Int {
		var round = 0
		var people = [Combatant]()
		for person in input.startPeople {
			people.append(Combatant(person.kind, position: person.position))
		}
		let map = input.map
		
		print(round)
		printMap(map, people: people)
		while (people.filter {$0.kind == .elf}).count > 0
				&& (people.filter {$0.kind == .goblin}).count > 0 {
			let fullRound = playRound(map: map, people: &people)
			if fullRound {
				round += 1
				print(round)
			}
			printMap(map, people: people)
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
	
	private func solvePartTwo(_ input: ParseResult) -> Int {
		if input.endPeople == nil {
			// Jumping ahead for the challenge. This is the actual amount
			Combatant.elfAttackPowerBoost = 17
		}
		
		while true {
			print("Elf attack bonus \(Combatant.elfAttackPowerBoost)")
			var people = [Combatant]()
			for person in input.startPeople {
				people.append(Combatant(person.kind, position: person.position))
			}
			let startingElfCount = (people.filter {$0.kind == .elf}).count
			var round = 0
			//print(round)

			var elfCount = startingElfCount
			var goblinCount = (people.filter {$0.kind == .goblin}).count

			while elfCount == startingElfCount && goblinCount > 0 {
				let fullRound = playRound(map: input.map, people: &people)
				elfCount = (people.filter {$0.kind == .elf}).count
				goblinCount = (people.filter {$0.kind == .goblin}).count
				if fullRound { round += 1 }
			}
			
			printMap(input.map, people: people)
			
			if startingElfCount == elfCount {
				// Have eliminated goblins without losing an elf
				return calcScore(numberOfRounds: round, people: people)
			}
			
			Combatant.elfAttackPowerBoost += 1
		}
	}
	
	private func playRound(map: AoCGrid2D, people: inout [Combatant]) -> Bool {
		var killed = Set<Combatant>()
		var fullRound = true
		
		// Sort people into reading order (top to bottom, left to right)
		people.sort(by: Combatant.readingOrderSort(c0:c1:))
		
		for person in people {
			if killed.contains(person) { continue }
			let alivePeople = people.filter { !killed.contains($0) }
			
			// Find targets
			let allTargets = alivePeople.filter { $0.kind != person.kind }
			if allTargets.count == 0 { fullRound = false }
			
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
				//print("Person at \(person.position)")
				var coordToMoveTo: AoCCoord2D?
				var lowestCost = Int.max
				let coordsSortedByReadingOrder = coordsInRangeOfTargets.sorted(by: AoCCoord2D.readingOrderSort(c0:c1:))
				for coord in coordsSortedByReadingOrder {
					if coord.manhattanDistance(to: person.position) > lowestCost {
						// There is no way for the coord to have a cost less than lowestCost
						continue
					}
					if let lcp = findLeastCostPath(in: map, from: person.position, to: coord,
												   barrierValue: "#",
												   additionalBarriers: otherPeopleCoords) {
						if lcp.cost < lowestCost {
							coordToMoveTo = lcp.firstMove
							lowestCost = lcp.cost
						}
					}
				}
				if let coordToMoveTo = coordToMoveTo {
					print("\(person.kind.rawValue) at \(person.position) moving to \(coordToMoveTo)")
					person.position = coordToMoveTo
				}
			}
			
			if coordsInRangeOfTargets.contains(person.position) == true {
				// Attack the weakest target in range
				var targetsInRange = allTargets.filter { $0.position.isAdjacent(to: person.position) }
				targetsInRange.sort { $0.hp < $1.hp }
				let weakestTarget = targetsInRange.first!
				weakestTarget.hp -= person.attackPower
				if weakestTarget.hp <= 0 {
					killed.insert(weakestTarget)
					print("\(weakestTarget.kind) killed at \(weakestTarget.position)")
				}
			}
		}
		
		people.removeAll { killed.contains($0) }
		return fullRound
	}
	
	private func findLeastCostPath(in map: AoCGrid2D,
								   from source: AoCCoord2D,
								   to target: AoCCoord2D,
								   barrierValue: String,
								   additionalBarriers: Set<AoCCoord2D>?) -> (cost: Int, firstMove: AoCCoord2D)? {
		// Assuming constant cost for now
		struct Node {
			var cost = 1
			var minTravelCost = Int.max
			var visited = false
			var isBarrier = false
		}
		// extent might not have origin at 0,0
		// so there are corrections below to translate the grid
		let ext = map.extent
		var grid = [[Node]](repeating: [Node](repeating: Node(), count: ext.width), count: ext.height)
		// Mark all barriers
		for (r, c) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let coord = AoCCoord2D(x: c, y: r)
			if map.value(at: coord) == barrierValue || (additionalBarriers != nil && additionalBarriers!.contains(coord)) {
				grid[r - ext.min.y][c - ext.min.x].isBarrier = true
			}
		}
		//print(">> Finding path with source \(source) and target \(target)")

		// Going to calculate using dijkstra but STARTING at the target.
		// That way the cost is calculated to the source and the next-least-cost squares
		// will be next to the source. Doing it the other way (starting at source) left
		// some low-cost dead ends next to source and it was hard to choose which way to go.
		let endPos = AoCCoord2D(x: source.x - ext.min.x, y: source.y - ext.min.y)
		let startPos = AoCCoord2D(x: target.x - ext.min.x, y: target.y - ext.min.y)
		
		var pos = startPos
		grid[pos.y][pos.x].minTravelCost = 0
		
		while grid[endPos.y][endPos.x].visited == false {
			for n in pos.getAdjacent() {
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
		
		// Print for debugging
//		for r in grid {
//			let rTemp = r.map { $0.minTravelCost == Int.max ? "X" : String($0.minTravelCost) }
//			print(rTemp.joined(separator: " "))
//		}
//		print()
		
		if grid[endPos.y][endPos.x].visited == false {
			//print("No path with source \(source) and target \(target)")
			return nil // Could not find a path
		}
		
		let leastCostForTotalPath = grid[endPos.y][endPos.x].minTravelCost
		var firstMoves = endPos.getAdjacent().filter { grid[$0.y][$0.x].visited }
		if firstMoves.count > 1 {
			firstMoves.sort { grid[$0.y][$0.x].minTravelCost < grid[$1.y][$1.x].minTravelCost }
			let min = grid[firstMoves[0].y][firstMoves[0].x].minTravelCost
			firstMoves.removeAll { grid[$0.y][$0.x].minTravelCost != min }
			firstMoves.sort(by: AoCCoord2D.readingOrderSort(c0:c1:))
		}
		//print("Best first move is to \(firstMoves.first!) with cost \(grid[firstMoves.first!.y][firstMoves.first!.x].minTravelCost)")
		return (cost: leastCostForTotalPath, firstMove: firstMoves.first!)
	}

	private func calcScore(numberOfRounds: Int, people: [Combatant]) -> Int {
		let sum = (people.map { $0.hp }).reduce(0, +)
		print("Score: \(sum * numberOfRounds). \(numberOfRounds) rounds, \(sum) hit points remaining.")
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
		let map = AoCGrid2D()
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
		
		if people.count > 1 {
			result.endPeople = people[1]
		}
		return result
	}
	
	private func printMap(_ map: AoCGrid2D, people: [Combatant]) {
		var markers = Dictionary<AoCCoord2D, String>()
		for person in people {
			markers[person.position] = person.kind.rawValue
		}
		map.draw(markers: markers)
		for person in people.sorted(by: { p1, p2 in
			AoCCoord2D.readingOrderSort(c0: p1.position, c1: p2.position)
		}) {
			print(person.description)
		}
	}
}

private class Combatant: Hashable {
	static let BASE_ATTACK_POWER = 3
	static var elfAttackPowerBoost = 0
	
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
	
	var description: String {
		return "\(kind.rawValue)(\(hp))"
	}
	
	var attackPower: Int {
		switch kind {
		case .elf:
			return Combatant.BASE_ATTACK_POWER + Combatant.elfAttackPowerBoost
		case .goblin:
			return Combatant.BASE_ATTACK_POWER
		}
	}
}
