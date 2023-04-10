class Stuff
  ##
  # @param hello [Object] When the work type does not have =part_of= we When
  #        thework type does not have =part_of= weWhen the work type does not have
  #        =part_of= we When the work type does not have =part_of= we
  # @param world [Object] When the work type does not have =part_of= we When the
  #        work type does not have =part_of= weWhen the work type does not have
  #        =part_of= we When the work type does not have =part_of= we
  def initialize(hello:, world:)
    self.hello = hello
    self.world = world
  end

  attr_accessor :hello, :world
  private :hello, :world
  private :hello=, :world=
end
