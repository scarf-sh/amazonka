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
-- Module      : Amazonka.KinesisAnalyticsV2.ListApplicationVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions for the specified application, including versions
-- that were rolled back. The response also includes a summary of the
-- configuration associated with each version.
--
-- To get the complete description of a specific application version,
-- invoke the DescribeApplicationVersion operation.
--
-- This operation is supported only for Amazon Kinesis Data Analytics for
-- Apache Flink.
module Amazonka.KinesisAnalyticsV2.ListApplicationVersions
  ( -- * Creating a Request
    ListApplicationVersions (..),
    newListApplicationVersions,

    -- * Request Lenses
    listApplicationVersions_nextToken,
    listApplicationVersions_limit,
    listApplicationVersions_applicationName,

    -- * Destructuring the Response
    ListApplicationVersionsResponse (..),
    newListApplicationVersionsResponse,

    -- * Response Lenses
    listApplicationVersionsResponse_applicationVersionSummaries,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { -- | If a previous invocation of this operation returned a pagination token,
    -- pass it into this value to retrieve the next set of results. For more
    -- information about pagination, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of versions to list in this invocation of the
    -- operation.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the application for which you want to list all versions.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationVersions_nextToken' - If a previous invocation of this operation returned a pagination token,
-- pass it into this value to retrieve the next set of results. For more
-- information about pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
--
-- 'limit', 'listApplicationVersions_limit' - The maximum number of versions to list in this invocation of the
-- operation.
--
-- 'applicationName', 'listApplicationVersions_applicationName' - The name of the application for which you want to list all versions.
newListApplicationVersions ::
  -- | 'applicationName'
  Prelude.Text ->
  ListApplicationVersions
newListApplicationVersions pApplicationName_ =
  ListApplicationVersions'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | If a previous invocation of this operation returned a pagination token,
-- pass it into this value to retrieve the next set of results. For more
-- information about pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
listApplicationVersions_nextToken :: Lens.Lens' ListApplicationVersions (Prelude.Maybe Prelude.Text)
listApplicationVersions_nextToken = Lens.lens (\ListApplicationVersions' {nextToken} -> nextToken) (\s@ListApplicationVersions' {} a -> s {nextToken = a} :: ListApplicationVersions)

-- | The maximum number of versions to list in this invocation of the
-- operation.
listApplicationVersions_limit :: Lens.Lens' ListApplicationVersions (Prelude.Maybe Prelude.Natural)
listApplicationVersions_limit = Lens.lens (\ListApplicationVersions' {limit} -> limit) (\s@ListApplicationVersions' {} a -> s {limit = a} :: ListApplicationVersions)

-- | The name of the application for which you want to list all versions.
listApplicationVersions_applicationName :: Lens.Lens' ListApplicationVersions Prelude.Text
listApplicationVersions_applicationName = Lens.lens (\ListApplicationVersions' {applicationName} -> applicationName) (\s@ListApplicationVersions' {} a -> s {applicationName = a} :: ListApplicationVersions)

instance Core.AWSRequest ListApplicationVersions where
  type
    AWSResponse ListApplicationVersions =
      ListApplicationVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationVersionsResponse'
            Prelude.<$> ( x Core..?> "ApplicationVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationVersions where
  hashWithSalt salt' ListApplicationVersions' {..} =
    salt' `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApplicationVersions where
  rnf ListApplicationVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListApplicationVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.ListApplicationVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListApplicationVersions where
  toJSON ListApplicationVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("ApplicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath ListApplicationVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListApplicationVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { -- | A list of the application versions and the associated configuration
    -- summaries. The list includes application versions that were rolled back.
    --
    -- To get the complete description of a specific application version,
    -- invoke the DescribeApplicationVersion operation.
    applicationVersionSummaries :: Prelude.Maybe [ApplicationVersionSummary],
    -- | The pagination token for the next set of results, or @null@ if there are
    -- no additional results. To retrieve the next set of items, pass this
    -- token into a subsequent invocation of this operation. For more
    -- information about pagination, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersionSummaries', 'listApplicationVersionsResponse_applicationVersionSummaries' - A list of the application versions and the associated configuration
-- summaries. The list includes application versions that were rolled back.
--
-- To get the complete description of a specific application version,
-- invoke the DescribeApplicationVersion operation.
--
-- 'nextToken', 'listApplicationVersionsResponse_nextToken' - The pagination token for the next set of results, or @null@ if there are
-- no additional results. To retrieve the next set of items, pass this
-- token into a subsequent invocation of this operation. For more
-- information about pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
--
-- 'httpStatus', 'listApplicationVersionsResponse_httpStatus' - The response's http status code.
newListApplicationVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationVersionsResponse
newListApplicationVersionsResponse pHttpStatus_ =
  ListApplicationVersionsResponse'
    { applicationVersionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the application versions and the associated configuration
-- summaries. The list includes application versions that were rolled back.
--
-- To get the complete description of a specific application version,
-- invoke the DescribeApplicationVersion operation.
listApplicationVersionsResponse_applicationVersionSummaries :: Lens.Lens' ListApplicationVersionsResponse (Prelude.Maybe [ApplicationVersionSummary])
listApplicationVersionsResponse_applicationVersionSummaries = Lens.lens (\ListApplicationVersionsResponse' {applicationVersionSummaries} -> applicationVersionSummaries) (\s@ListApplicationVersionsResponse' {} a -> s {applicationVersionSummaries = a} :: ListApplicationVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the next set of results, or @null@ if there are
-- no additional results. To retrieve the next set of items, pass this
-- token into a subsequent invocation of this operation. For more
-- information about pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
listApplicationVersionsResponse_nextToken :: Lens.Lens' ListApplicationVersionsResponse (Prelude.Maybe Prelude.Text)
listApplicationVersionsResponse_nextToken = Lens.lens (\ListApplicationVersionsResponse' {nextToken} -> nextToken) (\s@ListApplicationVersionsResponse' {} a -> s {nextToken = a} :: ListApplicationVersionsResponse)

-- | The response's http status code.
listApplicationVersionsResponse_httpStatus :: Lens.Lens' ListApplicationVersionsResponse Prelude.Int
listApplicationVersionsResponse_httpStatus = Lens.lens (\ListApplicationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationVersionsResponse' {} a -> s {httpStatus = a} :: ListApplicationVersionsResponse)

instance
  Prelude.NFData
    ListApplicationVersionsResponse
  where
  rnf ListApplicationVersionsResponse' {..} =
    Prelude.rnf applicationVersionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
