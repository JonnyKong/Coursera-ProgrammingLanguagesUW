# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
	def initialize (point_array, board)
		super
		@rotation_index = 0
	end

	All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),# Z
               rotations([[0,0], [-1, 0], [-1, 1], [0, 1], [1, 0]]),
               [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [1, 0], [0, -1]])] # lil L

    def self.next_piece (board)
    	MyPiece.new(All_My_Pieces.sample, board)
    end

    def self.cheat_piece (board)
    	MyPiece.new([[[0,0]]], board)
    end
end

class MyBoard < Board
	def initialize (game)
		super
		@current_block = MyPiece.next_piece(self)
		@cheated = false
	end
	def flip
		if !game_over? and @game.is_running?
      		@current_block.move(0, 0, 2)
      	end
    	draw
  	end
  	def cheat
  		if !game_over? and @game.is_running? and @score > 100 and @cheated == false
  			@score -= 100
  			@cheated = true
  		end
  	end
  	def next_piece
  		if @cheated
    		@current_block = MyPiece.cheat_piece(self)
    		@cheated = false
    	else
    		@current_block = MyPiece.next_piece(self)
    	end
    	@current_pos = nil
    end
    def store_current
    	locations = @current_block.current_rotation
    	displacement = @current_block.position
    	(0..(locations.size - 1)).each{|index| 
      	current = locations[index];
      	@grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      	@current_pos[index]
    	}
    	remove_filled
    	@delay = [@delay - 2, 80].max
  	end
end

class MyTetris < Tetris
	def initialize
		super
		keybinding
	end

	def keybinding
		@root.bind('u', proc {@board.flip})
		@root.bind('c', proc {@board.cheat})
	end

  	def set_board
    	@canvas = TetrisCanvas.new
    	@board = MyBoard.new(self)
    	@canvas.place(@board.block_size * @board.num_rows + 3,
                  	@board.block_size * @board.num_columns + 6, 24, 80)
    	@board.draw
  	end
end