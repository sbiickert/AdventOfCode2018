//
//  ResultView.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-04.
//

import SwiftUI

struct ResultView: View {
	var input: AoCInput
	@State private var result: AoCResult?
	@State private var running = false
    var body: some View {
		VStack {
			Button {
				running = true
				result = input.solution.solve(filename: input.fileName, index: input.index)
				running = false
			} label: {
				Text("Solve Day \(input.solution.day)")
			}
			.disabled(running)
			.padding()

			if (running) { AnyView(ProgressView()) } else { AnyView(EmptyView())}
			
			VStack(alignment: .leading) {
				Text("Part One").bold()
				HStack {
					Image(systemName: result?.part1 != nil ? "checkmark.seal" : "questionmark.diamond")
						.resizable(capInsets: EdgeInsets(top: 0.0, leading: 0.0, bottom: 0.0, trailing: 0.0))
						.foregroundColor(result?.part1 != nil ? Color.green : Color.gray)
						.padding(.all, 5.0)
						.frame(width: 32, height: 32, alignment: .center)
					Text(result?.part1 ?? "No answer")
				}
				Divider()
				Text("Part Two").bold()
				HStack {
					Image(systemName: result?.part2 != nil ? "checkmark.seal" : "questionmark.diamond")
						.resizable(capInsets: EdgeInsets(top: 0.0, leading: 0.0, bottom: 0.0, trailing: 0.0))
						.foregroundColor(result?.part2 != nil ? Color.green : Color.gray)
						.padding(.all, 5.0)
						.frame(width: 32, height: 32, alignment: .center)
					Text(result?.part2 ?? "No answer")
				}
			}
		}.padding()
   }
}

struct ResultView_Previews: PreviewProvider {
    static var previews: some View {
		ResultView(input: inputs(for: solutions[2])[0])
    }
}
