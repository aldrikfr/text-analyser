text = "
    Ruby is a great programming language. It is object oriented
    and has many groovy features. Some people don't like it, but that's
    not our problem! It's easy to learn. It's great. To learn more about Ruby,
    visit the official Ruby website today.
    "
sentences = text.gsub(/\s+/, ' ').strip.split(/\.|\?|!/)
sentences_sorted = sentences.sort_by { |sentence| sentence.length }
one_third = sentences_sorted.length / 3
ideal_sentences = sentences_sorted.slice(one_third, one_third + 1)
ideal_sentences = ideal_sentences.select do |sentence|
  sentence =~ /is|are/
end
puts ideal_sentences.join('. ')
