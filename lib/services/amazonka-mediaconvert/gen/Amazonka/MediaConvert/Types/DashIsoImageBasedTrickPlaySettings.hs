{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.DashIsoIntervalCadence
import qualified Amazonka.Prelude as Prelude

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
--
-- /See:/ 'newDashIsoImageBasedTrickPlaySettings' smart constructor.
data DashIsoImageBasedTrickPlaySettings = DashIsoImageBasedTrickPlaySettings'
  { -- | Number of thumbnails in each row of a tile image. Set a value between 1
    -- and 512.
    tileWidth :: Prelude.Maybe Prelude.Natural,
    -- | Height of each thumbnail within each tile image, in pixels. Leave blank
    -- to maintain aspect ratio with thumbnail width. If following the aspect
    -- ratio would lead to a total tile height greater than 4096, then the job
    -- will be rejected. Must be divisible by 2.
    thumbnailHeight :: Prelude.Maybe Prelude.Natural,
    -- | The cadence MediaConvert follows for generating thumbnails. If set to
    -- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
    -- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
    -- MediaConvert generates thumbnails according to the interval you specify
    -- in thumbnailInterval.
    intervalCadence :: Prelude.Maybe DashIsoIntervalCadence,
    -- | Width of each thumbnail within each tile image, in pixels. Default is
    -- 312. Must be divisible by 8.
    thumbnailWidth :: Prelude.Maybe Prelude.Natural,
    -- | Enter the interval, in seconds, that MediaConvert uses to generate
    -- thumbnails. If the interval you enter doesn\'t align with the output
    -- frame rate, MediaConvert automatically rounds the interval to align with
    -- the output frame rate. For example, if the output frame rate is 29.97
    -- frames per second and you enter 5, MediaConvert uses a 150 frame
    -- interval to generate thumbnails.
    thumbnailInterval :: Prelude.Maybe Prelude.Double,
    -- | Number of thumbnails in each column of a tile image. Set a value between
    -- 2 and 2048. Must be divisible by 2.
    tileHeight :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashIsoImageBasedTrickPlaySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tileWidth', 'dashIsoImageBasedTrickPlaySettings_tileWidth' - Number of thumbnails in each row of a tile image. Set a value between 1
-- and 512.
--
-- 'thumbnailHeight', 'dashIsoImageBasedTrickPlaySettings_thumbnailHeight' - Height of each thumbnail within each tile image, in pixels. Leave blank
-- to maintain aspect ratio with thumbnail width. If following the aspect
-- ratio would lead to a total tile height greater than 4096, then the job
-- will be rejected. Must be divisible by 2.
--
-- 'intervalCadence', 'dashIsoImageBasedTrickPlaySettings_intervalCadence' - The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
--
-- 'thumbnailWidth', 'dashIsoImageBasedTrickPlaySettings_thumbnailWidth' - Width of each thumbnail within each tile image, in pixels. Default is
-- 312. Must be divisible by 8.
--
-- 'thumbnailInterval', 'dashIsoImageBasedTrickPlaySettings_thumbnailInterval' - Enter the interval, in seconds, that MediaConvert uses to generate
-- thumbnails. If the interval you enter doesn\'t align with the output
-- frame rate, MediaConvert automatically rounds the interval to align with
-- the output frame rate. For example, if the output frame rate is 29.97
-- frames per second and you enter 5, MediaConvert uses a 150 frame
-- interval to generate thumbnails.
--
-- 'tileHeight', 'dashIsoImageBasedTrickPlaySettings_tileHeight' - Number of thumbnails in each column of a tile image. Set a value between
-- 2 and 2048. Must be divisible by 2.
newDashIsoImageBasedTrickPlaySettings ::
  DashIsoImageBasedTrickPlaySettings
newDashIsoImageBasedTrickPlaySettings =
  DashIsoImageBasedTrickPlaySettings'
    { tileWidth =
        Prelude.Nothing,
      thumbnailHeight = Prelude.Nothing,
      intervalCadence = Prelude.Nothing,
      thumbnailWidth = Prelude.Nothing,
      thumbnailInterval = Prelude.Nothing,
      tileHeight = Prelude.Nothing
    }

-- | Number of thumbnails in each row of a tile image. Set a value between 1
-- and 512.
dashIsoImageBasedTrickPlaySettings_tileWidth :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
dashIsoImageBasedTrickPlaySettings_tileWidth = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {tileWidth} -> tileWidth) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {tileWidth = a} :: DashIsoImageBasedTrickPlaySettings)

-- | Height of each thumbnail within each tile image, in pixels. Leave blank
-- to maintain aspect ratio with thumbnail width. If following the aspect
-- ratio would lead to a total tile height greater than 4096, then the job
-- will be rejected. Must be divisible by 2.
dashIsoImageBasedTrickPlaySettings_thumbnailHeight :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
dashIsoImageBasedTrickPlaySettings_thumbnailHeight = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {thumbnailHeight} -> thumbnailHeight) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {thumbnailHeight = a} :: DashIsoImageBasedTrickPlaySettings)

-- | The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
dashIsoImageBasedTrickPlaySettings_intervalCadence :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe DashIsoIntervalCadence)
dashIsoImageBasedTrickPlaySettings_intervalCadence = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {intervalCadence} -> intervalCadence) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {intervalCadence = a} :: DashIsoImageBasedTrickPlaySettings)

-- | Width of each thumbnail within each tile image, in pixels. Default is
-- 312. Must be divisible by 8.
dashIsoImageBasedTrickPlaySettings_thumbnailWidth :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
dashIsoImageBasedTrickPlaySettings_thumbnailWidth = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {thumbnailWidth} -> thumbnailWidth) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {thumbnailWidth = a} :: DashIsoImageBasedTrickPlaySettings)

-- | Enter the interval, in seconds, that MediaConvert uses to generate
-- thumbnails. If the interval you enter doesn\'t align with the output
-- frame rate, MediaConvert automatically rounds the interval to align with
-- the output frame rate. For example, if the output frame rate is 29.97
-- frames per second and you enter 5, MediaConvert uses a 150 frame
-- interval to generate thumbnails.
dashIsoImageBasedTrickPlaySettings_thumbnailInterval :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Double)
dashIsoImageBasedTrickPlaySettings_thumbnailInterval = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {thumbnailInterval} -> thumbnailInterval) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {thumbnailInterval = a} :: DashIsoImageBasedTrickPlaySettings)

-- | Number of thumbnails in each column of a tile image. Set a value between
-- 2 and 2048. Must be divisible by 2.
dashIsoImageBasedTrickPlaySettings_tileHeight :: Lens.Lens' DashIsoImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
dashIsoImageBasedTrickPlaySettings_tileHeight = Lens.lens (\DashIsoImageBasedTrickPlaySettings' {tileHeight} -> tileHeight) (\s@DashIsoImageBasedTrickPlaySettings' {} a -> s {tileHeight = a} :: DashIsoImageBasedTrickPlaySettings)

instance
  Core.FromJSON
    DashIsoImageBasedTrickPlaySettings
  where
  parseJSON =
    Core.withObject
      "DashIsoImageBasedTrickPlaySettings"
      ( \x ->
          DashIsoImageBasedTrickPlaySettings'
            Prelude.<$> (x Core..:? "tileWidth")
            Prelude.<*> (x Core..:? "thumbnailHeight")
            Prelude.<*> (x Core..:? "intervalCadence")
            Prelude.<*> (x Core..:? "thumbnailWidth")
            Prelude.<*> (x Core..:? "thumbnailInterval")
            Prelude.<*> (x Core..:? "tileHeight")
      )

instance
  Prelude.Hashable
    DashIsoImageBasedTrickPlaySettings
  where
  hashWithSalt
    salt'
    DashIsoImageBasedTrickPlaySettings' {..} =
      salt' `Prelude.hashWithSalt` tileHeight
        `Prelude.hashWithSalt` thumbnailInterval
        `Prelude.hashWithSalt` thumbnailWidth
        `Prelude.hashWithSalt` intervalCadence
        `Prelude.hashWithSalt` thumbnailHeight
        `Prelude.hashWithSalt` tileWidth

instance
  Prelude.NFData
    DashIsoImageBasedTrickPlaySettings
  where
  rnf DashIsoImageBasedTrickPlaySettings' {..} =
    Prelude.rnf tileWidth
      `Prelude.seq` Prelude.rnf tileHeight
      `Prelude.seq` Prelude.rnf thumbnailInterval
      `Prelude.seq` Prelude.rnf thumbnailWidth
      `Prelude.seq` Prelude.rnf intervalCadence
      `Prelude.seq` Prelude.rnf thumbnailHeight

instance
  Core.ToJSON
    DashIsoImageBasedTrickPlaySettings
  where
  toJSON DashIsoImageBasedTrickPlaySettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tileWidth" Core..=) Prelude.<$> tileWidth,
            ("thumbnailHeight" Core..=)
              Prelude.<$> thumbnailHeight,
            ("intervalCadence" Core..=)
              Prelude.<$> intervalCadence,
            ("thumbnailWidth" Core..=)
              Prelude.<$> thumbnailWidth,
            ("thumbnailInterval" Core..=)
              Prelude.<$> thumbnailInterval,
            ("tileHeight" Core..=) Prelude.<$> tileHeight
          ]
      )
