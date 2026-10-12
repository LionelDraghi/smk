-- -----------------------------------------------------------------------------
-- SPDX-FileCopyrightText: 2018, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------
-- This file is part of the List_Image project
-- available at https://github.com/LionelDraghi/List_Image
-- -----------------------------------------------------------------------------

package List_Image.Unix_Predefined_Styles is

   EOL : constant String := Unix_EOL;
   -- Note that two identical packages exist, named:
   -- List_Image.[Unix|Windows]_Predefined_Styles
   -- The only difference between those packages is this platform specific
   -- EOL definition.

   -- --------------------------------------------------------------------------
   --                         Predefined multi-line style
   -- --------------------------------------------------------------------------
   --
   -- - Bulleted_List_Style:
   --   > - A
   --   > - B
   --   > - C
   --
   -- - Markdown_Bulleted_List_Style:
   --   Like the bulleted list, but surrounded by
   --   two empty lines (in some Markdown implementations, if the first bullet
   --   is not preceded by an empty line, the list is not recognized)
   --
   -- - HTML_Bulleted_List_Style:
   --   > <ul>
   --   > <li>A</li>
   --   > <li>B</li>
   --   > <li>C</li>
   --   > </ul>
   --   Note: <ul></ul>, an empty list, is recognized by most browsers,
   --          but seems to be illegal HTML.
   --          No problem here, thanks to the If_Empty parameters nothing
   --          will be generated if the list is empty.
   --
   -- --------------------------------------------------------------------------

   package Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL,
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

   package Markdown_Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL & EOL,
      Prefix_If_Empty  => EOL,
      Postfix_If_Empty => "");

   package HTML_Bulleted_List_Style is new Image_Style
     (Prefix           => "<ul>"  & EOL & "  <li>",
      Separator        => "</li>" & EOL & "  <li>",
      Postfix          => "</li>" & EOL & "</ul>",
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

end List_Image.Unix_Predefined_Styles;
