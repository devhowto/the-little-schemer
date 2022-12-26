# vim: set filetype=ruby:

#
# $ bundle install
# $ bundle exec guard --guardfile ./Guardfile
#

require 'asciidoctor'
require 'awesome_print'

file = 'the-little-schemer.adoc'

guard 'shell' do
  watch(/\.(adoc)$/) { |m|
    puts '-' * 80
    ap m
    puts '-' * 80

    Asciidoctor.convert_file(
      file,
      :safe => :unsafe,
      :to_file => "#{file}.html",
      :doctype => :article
    )
  }
end

