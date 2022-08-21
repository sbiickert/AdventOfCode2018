import Foundation

class Day07: AoCSolution {
	var jobJar: JobJar?
	var letterValues = Dictionary<String, Int>()

	override init() {
		super.init()
		day = 7
		name = "The Sum of Its Parts"
		for i in 0..<26 {
			letterValues[AoCUtil.ALPHABET[i].uppercased()] = i+1
		}
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let precedence = parseInstructionOrder(input)
		
		jobJar = JobJar(precedence)
		let result1 = solvePartOne()
		print("Part One: the order of steps is \(result1)")

		jobJar = JobJar(precedence)
		let result2 = solvePartTwo(nWorkers: input.count < 10 ? 2 : 5)
		print("Part Two: the time to complete the job is \(result2)")

		return AoCResult(part1: result1, part2: result2)
	}
	
	private func solvePartOne() -> String {
		var order = [String]()

		while let job = jobJar?.nextJob() {
			order.append(job)
			jobJar?.complete(job: job)
		}
		
		return order.joined()
	}
	
	private func solvePartTwo(nWorkers: Int) -> String {
		guard jobJar != nil else { return "Job jar was nil."}
		
		var order = [String]()
		var wip = [(String?,Int)](repeating: (nil,0), count: nWorkers)
		var time = 0

		if let jobJar = jobJar {
			while jobJar.hasJobs || wip.filter({ $0.0 != nil }).count > 0 {
				for w in 0..<wip.count {
					// Look for a completed job
					if wip[w].0 != nil && wip[w].1 == 0 {
						order.append(wip[w].0!)
						jobJar.complete(job: wip[w].0!)
						wip[w].0 = nil
						
					}
				}
				for w in 0..<wip.count {
					if wip[w].0 == nil,
					   let nextJob = jobJar.nextJob() {
						wip[w] = (nextJob, letterValues[nextJob]!)
						if nWorkers > 2 {wip[w].1 += 60}
					}
				}
				
				//print("\(time)\t\(wip.compactMap({ $0.0 == nil ? " " : $0.0 }).joined(separator: "\t"))")
				
				// Time advances
				for w in 0..<wip.count {
					if wip[w].0 != nil {
						// One second closer to completion
						wip[w].1 -= 1
					}
				}
				time += 1
			}
			
			print(order.joined())
		}

		
		return String(time-1)
	}
	
	
	private func parseInstructionOrder(_ input: [String]) -> [(String,String)] {
		// Ex. "Step C must be finished before step A can begin."
		let regex = try! NSRegularExpression(pattern: " ([A-Z]) must be finished before step ([A-Z]) ")
		
		var result = [(String, String)]()
		for line in input {
			let pm = regex.positionalMatches(line)
			result.append( (pm[0], pm[1]) )
		}
		
		return result
	}
}

class JobJar {
	private let END = "End"
	private var followersOf = Dictionary<String, [String]>()
	private var precedentsFor = Dictionary<String,[String]>()
	private var wip = Set<String>()

	init(_ input: [(String,String)]) {
		followersOf.removeAll()
		precedentsFor.removeAll()
		wip.removeAll()
		
		for item in input {
			if followersOf.keys.contains(item.0) == false { followersOf[item.0] = [String]() }
			followersOf[item.0]!.append(item.1)
			if precedentsFor.keys.contains(item.1) == false { precedentsFor[item.1] = [String]() }
			precedentsFor[item.1]!.append(item.0)
		}
		
		let noFollowers = precedentsFor.keys.filter { followersOf.keys.contains($0) == false }
		assert(noFollowers.count == 1)
		
		followersOf[noFollowers.first!] = [END]
		precedentsFor[END] = [noFollowers.first!]
	}
	
	var hasJobs: Bool {
		return followersOf.count > 0
	}
	
	func complete(job: String) {
		if followersOf.keys.contains(job) {
			for follower in followersOf[job]! {
				precedentsFor[follower] = precedentsFor[follower]?.filter { $0 != job }
				if precedentsFor[follower]!.count == 0 { precedentsFor.removeValue(forKey: follower) }
			}
			followersOf.removeValue(forKey: job)
		}
		wip.remove(job)
	}
	
	func nextJob() -> String? {
		var result: String?
		
		if followersOf.count > 0 {
			var jobsWithNoPrecedents = [String]()
			for job in followersOf.keys {
				if precedentsFor.keys.contains(job) == false {
					jobsWithNoPrecedents.append(job)
				}
			}
			for step in jobsWithNoPrecedents.sorted() {
				if wip.contains(step) == false {
					result = step
					break
				}
			}
		}
		
		if let result = result {
			if result == END {
				followersOf.removeAll()
				precedentsFor.removeAll()
				return nil
			}
			else {
				wip.insert(result)
			}
		}
		
		return result
	}

}
