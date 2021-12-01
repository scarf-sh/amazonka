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
-- Module      : Amazonka.IoTWireless.ListDeviceProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the device profiles registered to your AWS account.
module Amazonka.IoTWireless.ListDeviceProfiles
  ( -- * Creating a Request
    ListDeviceProfiles (..),
    newListDeviceProfiles,

    -- * Request Lenses
    listDeviceProfiles_nextToken,
    listDeviceProfiles_maxResults,

    -- * Destructuring the Response
    ListDeviceProfilesResponse (..),
    newListDeviceProfilesResponse,

    -- * Response Lenses
    listDeviceProfilesResponse_deviceProfileList,
    listDeviceProfilesResponse_nextToken,
    listDeviceProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceProfiles' smart constructor.
data ListDeviceProfiles = ListDeviceProfiles'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceProfiles_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listDeviceProfiles_maxResults' - The maximum number of results to return in this operation.
newListDeviceProfiles ::
  ListDeviceProfiles
newListDeviceProfiles =
  ListDeviceProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listDeviceProfiles_nextToken :: Lens.Lens' ListDeviceProfiles (Prelude.Maybe Prelude.Text)
listDeviceProfiles_nextToken = Lens.lens (\ListDeviceProfiles' {nextToken} -> nextToken) (\s@ListDeviceProfiles' {} a -> s {nextToken = a} :: ListDeviceProfiles)

-- | The maximum number of results to return in this operation.
listDeviceProfiles_maxResults :: Lens.Lens' ListDeviceProfiles (Prelude.Maybe Prelude.Natural)
listDeviceProfiles_maxResults = Lens.lens (\ListDeviceProfiles' {maxResults} -> maxResults) (\s@ListDeviceProfiles' {} a -> s {maxResults = a} :: ListDeviceProfiles)

instance Core.AWSRequest ListDeviceProfiles where
  type
    AWSResponse ListDeviceProfiles =
      ListDeviceProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceProfilesResponse'
            Prelude.<$> ( x Core..?> "DeviceProfileList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceProfiles where
  hashWithSalt salt' ListDeviceProfiles' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDeviceProfiles where
  rnf ListDeviceProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDeviceProfiles where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDeviceProfiles where
  toPath = Prelude.const "/device-profiles"

instance Core.ToQuery ListDeviceProfiles where
  toQuery ListDeviceProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDeviceProfilesResponse' smart constructor.
data ListDeviceProfilesResponse = ListDeviceProfilesResponse'
  { -- | The list of device profiles.
    deviceProfileList :: Prelude.Maybe [DeviceProfile],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceProfileList', 'listDeviceProfilesResponse_deviceProfileList' - The list of device profiles.
--
-- 'nextToken', 'listDeviceProfilesResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listDeviceProfilesResponse_httpStatus' - The response's http status code.
newListDeviceProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceProfilesResponse
newListDeviceProfilesResponse pHttpStatus_ =
  ListDeviceProfilesResponse'
    { deviceProfileList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of device profiles.
listDeviceProfilesResponse_deviceProfileList :: Lens.Lens' ListDeviceProfilesResponse (Prelude.Maybe [DeviceProfile])
listDeviceProfilesResponse_deviceProfileList = Lens.lens (\ListDeviceProfilesResponse' {deviceProfileList} -> deviceProfileList) (\s@ListDeviceProfilesResponse' {} a -> s {deviceProfileList = a} :: ListDeviceProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listDeviceProfilesResponse_nextToken :: Lens.Lens' ListDeviceProfilesResponse (Prelude.Maybe Prelude.Text)
listDeviceProfilesResponse_nextToken = Lens.lens (\ListDeviceProfilesResponse' {nextToken} -> nextToken) (\s@ListDeviceProfilesResponse' {} a -> s {nextToken = a} :: ListDeviceProfilesResponse)

-- | The response's http status code.
listDeviceProfilesResponse_httpStatus :: Lens.Lens' ListDeviceProfilesResponse Prelude.Int
listDeviceProfilesResponse_httpStatus = Lens.lens (\ListDeviceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListDeviceProfilesResponse' {} a -> s {httpStatus = a} :: ListDeviceProfilesResponse)

instance Prelude.NFData ListDeviceProfilesResponse where
  rnf ListDeviceProfilesResponse' {..} =
    Prelude.rnf deviceProfileList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
