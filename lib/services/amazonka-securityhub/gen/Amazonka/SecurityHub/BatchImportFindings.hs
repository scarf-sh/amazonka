{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.BatchImportFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports security findings generated from an integrated product into
-- Security Hub. This action is requested by the integrated product to
-- import its findings into Security Hub.
--
-- The maximum allowed size for a finding is 240 Kb. An error is returned
-- for any finding larger than 240 Kb.
--
-- After a finding is created, @BatchImportFindings@ cannot be used to
-- update the following finding fields and objects, which Security Hub
-- customers use to manage their investigation workflow.
--
-- -   @Note@
--
-- -   @UserDefinedFields@
--
-- -   @VerificationState@
--
-- -   @Workflow@
--
-- Finding providers also should not use @BatchImportFindings@ to update
-- the following attributes.
--
-- -   @Confidence@
--
-- -   @Criticality@
--
-- -   @RelatedFindings@
--
-- -   @Severity@
--
-- -   @Types@
--
-- Instead, finding providers use @FindingProviderFields@ to provide values
-- for these attributes.
module Amazonka.SecurityHub.BatchImportFindings
  ( -- * Creating a Request
    BatchImportFindings (..),
    newBatchImportFindings,

    -- * Request Lenses
    batchImportFindings_findings,

    -- * Destructuring the Response
    BatchImportFindingsResponse (..),
    newBatchImportFindingsResponse,

    -- * Response Lenses
    batchImportFindingsResponse_failedFindings,
    batchImportFindingsResponse_httpStatus,
    batchImportFindingsResponse_failedCount,
    batchImportFindingsResponse_successCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchImportFindings' smart constructor.
data BatchImportFindings = BatchImportFindings'
  { -- | A list of findings to import. To successfully import a finding, it must
    -- follow the
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>.
    -- Maximum of 100 findings per request.
    findings :: Prelude.NonEmpty AwsSecurityFinding
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchImportFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findings', 'batchImportFindings_findings' - A list of findings to import. To successfully import a finding, it must
-- follow the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>.
-- Maximum of 100 findings per request.
newBatchImportFindings ::
  -- | 'findings'
  Prelude.NonEmpty AwsSecurityFinding ->
  BatchImportFindings
newBatchImportFindings pFindings_ =
  BatchImportFindings'
    { findings =
        Lens.coerced Lens.# pFindings_
    }

-- | A list of findings to import. To successfully import a finding, it must
-- follow the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>.
-- Maximum of 100 findings per request.
batchImportFindings_findings :: Lens.Lens' BatchImportFindings (Prelude.NonEmpty AwsSecurityFinding)
batchImportFindings_findings = Lens.lens (\BatchImportFindings' {findings} -> findings) (\s@BatchImportFindings' {} a -> s {findings = a} :: BatchImportFindings) Prelude.. Lens.coerced

instance Core.AWSRequest BatchImportFindings where
  type
    AWSResponse BatchImportFindings =
      BatchImportFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchImportFindingsResponse'
            Prelude.<$> (x Core..?> "FailedFindings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FailedCount")
            Prelude.<*> (x Core..:> "SuccessCount")
      )

instance Prelude.Hashable BatchImportFindings where
  hashWithSalt salt' BatchImportFindings' {..} =
    salt' `Prelude.hashWithSalt` findings

instance Prelude.NFData BatchImportFindings where
  rnf BatchImportFindings' {..} = Prelude.rnf findings

instance Core.ToHeaders BatchImportFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchImportFindings where
  toJSON BatchImportFindings' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Findings" Core..= findings)]
      )

instance Core.ToPath BatchImportFindings where
  toPath = Prelude.const "/findings/import"

instance Core.ToQuery BatchImportFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchImportFindingsResponse' smart constructor.
data BatchImportFindingsResponse = BatchImportFindingsResponse'
  { -- | The list of findings that failed to import.
    failedFindings :: Prelude.Maybe [ImportFindingsError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of findings that failed to import.
    failedCount :: Prelude.Int,
    -- | The number of findings that were successfully imported.
    successCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchImportFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedFindings', 'batchImportFindingsResponse_failedFindings' - The list of findings that failed to import.
--
-- 'httpStatus', 'batchImportFindingsResponse_httpStatus' - The response's http status code.
--
-- 'failedCount', 'batchImportFindingsResponse_failedCount' - The number of findings that failed to import.
--
-- 'successCount', 'batchImportFindingsResponse_successCount' - The number of findings that were successfully imported.
newBatchImportFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'failedCount'
  Prelude.Int ->
  -- | 'successCount'
  Prelude.Int ->
  BatchImportFindingsResponse
newBatchImportFindingsResponse
  pHttpStatus_
  pFailedCount_
  pSuccessCount_ =
    BatchImportFindingsResponse'
      { failedFindings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        failedCount = pFailedCount_,
        successCount = pSuccessCount_
      }

-- | The list of findings that failed to import.
batchImportFindingsResponse_failedFindings :: Lens.Lens' BatchImportFindingsResponse (Prelude.Maybe [ImportFindingsError])
batchImportFindingsResponse_failedFindings = Lens.lens (\BatchImportFindingsResponse' {failedFindings} -> failedFindings) (\s@BatchImportFindingsResponse' {} a -> s {failedFindings = a} :: BatchImportFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchImportFindingsResponse_httpStatus :: Lens.Lens' BatchImportFindingsResponse Prelude.Int
batchImportFindingsResponse_httpStatus = Lens.lens (\BatchImportFindingsResponse' {httpStatus} -> httpStatus) (\s@BatchImportFindingsResponse' {} a -> s {httpStatus = a} :: BatchImportFindingsResponse)

-- | The number of findings that failed to import.
batchImportFindingsResponse_failedCount :: Lens.Lens' BatchImportFindingsResponse Prelude.Int
batchImportFindingsResponse_failedCount = Lens.lens (\BatchImportFindingsResponse' {failedCount} -> failedCount) (\s@BatchImportFindingsResponse' {} a -> s {failedCount = a} :: BatchImportFindingsResponse)

-- | The number of findings that were successfully imported.
batchImportFindingsResponse_successCount :: Lens.Lens' BatchImportFindingsResponse Prelude.Int
batchImportFindingsResponse_successCount = Lens.lens (\BatchImportFindingsResponse' {successCount} -> successCount) (\s@BatchImportFindingsResponse' {} a -> s {successCount = a} :: BatchImportFindingsResponse)

instance Prelude.NFData BatchImportFindingsResponse where
  rnf BatchImportFindingsResponse' {..} =
    Prelude.rnf failedFindings
      `Prelude.seq` Prelude.rnf successCount
      `Prelude.seq` Prelude.rnf failedCount
      `Prelude.seq` Prelude.rnf httpStatus
