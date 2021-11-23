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
     * This method updates board variable and throws if any illegal move occurred.
     * Otherwise method doesn't throw anything. Passed boards must contain only one
     * alteration of position (one tick).
     *
     * @throws MandatoryKillMove
     * @throws VerticalOrHorizontalMove
     * @throws MoreThanOneMoveMade
     * @throws OverlappingPieces
     * @throws JumpedOverSameColorPiece
     * @throws JumpedOverMoreThanOnePiece
     * @throws JumpedOverAlreadyKilledPiece
     * @throws MoreThanOneTileMove
     *
     * @param board next state of a board
     *
     * @return updated board to be replaced
     */
    public List<List<Piece>> update(List<List<Piece>> board)
            throws MoreThanOneMoveMade, VerticalOrHorizontalMove, MandatoryKillMove, OverlappingPieces,
            JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece, MoreThanOneTileMove;

    /**
     * This method updates board variable and throws if any illegal move occurred.
     * Otherwise method doesn't throw anything.
     * This method requires only coordinates of changed piece with specified piece affiliation.
     *
     * @throws MandatoryKillMove
     * @throws VerticalOrHorizontalMove
     * @throws MoreThanOneMoveMade
     * @throws OverlappingPieces
     * @throws JumpedOverSameColorPiece
     * @throws JumpedOverMoreThanOnePiece
     * @throws JumpedOverAlreadyKilledPiece
     * @throws MoreThanOneTileMove
     *
     * @param x coordinate
     * @param y coordinate
     * @param affiliation type of piece to be given
     *
     * @return updated board to be replaced
     */
    public List<List<Piece>> updateV2(Integer x, Integer y, Piece affiliation)
            throws MoreThanOneMoveMade, VerticalOrHorizontalMove, MandatoryKillMove, OverlappingPieces,
            JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece, MoreThanOneTileMove;
}
