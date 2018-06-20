--
-- ejabberd, Copyright (C) 2002-2018   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--

SET ANSI_PADDING OFF;
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;
SET ANSI_PADDING ON;

CREATE TABLE [dbo].[irc_custom] (
        [jid] [varchar] (255) NOT NULL,
        [host] [varchar] (255) NOT NULL,
        [data] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [irc_custom_jid_host] ON [irc_custom] (jid, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
