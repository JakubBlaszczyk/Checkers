package com.pk.logic;

import java.util.List;

import com.pk.logic.exceptions.JumpedOverAlreadyKilledPiece;
import com.pk.logic.exceptions.JumpedOverMoreThanOnePiece;
import com.pk.logic.exceptions.JumpedOverSameColorPiece;
import com.pk.logic.exceptions.MandatoryKillMove;
import com.pk.logic.exceptions.MoreThanOneMoveMade;
import com.pk.logic.exceptions.MoreThanOneTileMove;
import com.pk.logic.exceptions.OverlappingPieces;
import com.pk.logic.exceptions.VerticalOrHorizontalMove;

public interface Logic {
    /**
     * This method updates board variable and throws if any illegal move occured.
     * Otherwise method doesn't throw anything. Passed boards must contain only one
     * alteration of position (one tick).
     * 
     * @throws MandatoryKillMove
     * @throws VerticalOrHorizontalMove
     * @throws MoreThanOneMoveMade
     * @throws OverlappingPieces
     * @throws JumpedOverSameColorPiece
     * @throws JumpedOverMoreThanOnePiece
     * @throws JumpedOverAlreadyKiledPiece
     * @throws MoreThanOneTileMove
     * 
     * @param board next state of a board
     * 
     * @return returns whos move is next, 0 when black 1 when white
     */
    public Boolean update(List<List<Piece>> board)
            throws MoreThanOneMoveMade, VerticalOrHorizontalMove, MandatoryKillMove, OverlappingPieces,
            JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece, MoreThanOneTileMove;
}
