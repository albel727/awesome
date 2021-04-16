---------------------------------------------------------------------------
--- A graph widget.
--
-- The graph goes from left to right. To change this to right to left, use
-- a `wibox.container.mirror` widget. This can also be used to have data
-- shown from top to bottom.
--
-- To add text on top of the graph, use a `wibox.layout.stack` and a
-- `wibox.container.align` widgets.
--
-- To display the graph vertically, use a `wibox.container.rotate` widget.
--
--@DOC_wibox_widget_defaults_graph_EXAMPLE@
-- @author Julien Danjou &lt;julien@danjou.info&gt;
-- @copyright 2009 Julien Danjou
-- @widgetmod wibox.widget.graph
-- @supermodule wibox.widget.base
---------------------------------------------------------------------------

local setmetatable = setmetatable
local ipairs = ipairs
local math = math
local table = table
local color = require("gears.color")
local base = require("wibox.widget.base")
local beautiful = require("beautiful")

local graph = { mt = {} }

--- Set the graph border_width.
--
--@DOC_wibox_widget_graph_border_width_EXAMPLE@
--
-- @property border_width
-- @tparam number border_width
-- @propemits true false
-- @see border_color

--- Set the graph border color.
--
-- If the value is nil, no border will be drawn.
--
--@DOC_wibox_widget_graph_border_color_EXAMPLE@
--
-- @property border_color
-- @tparam gears.color border_color The border color to set.
-- @propbeautiful
-- @propemits true false
-- @see gears.color

--- Set the graph foreground color.
--
--@DOC_wibox_widget_graph_color_EXAMPLE@
--
-- @property color
-- @tparam color color The graph color.
-- @usebeautiful beautiful.graph_fg
-- @propemits true false
-- @see gears.color

--- Set the graph background color.
--
--@DOC_wibox_widget_graph_background_color_EXAMPLE@
--
-- @property background_color
-- @tparam gears.color background_color The graph background color.
-- @usebeautiful beautiful.graph_bg
-- @propemits true false
-- @see gears.color

--- Set the maximum value the graph should handle.
--
-- @DOC_wibox_widget_graph_max_value_EXAMPLE@
--
-- If "scale" is also set, the graph never scales up below this value, but it
-- automatically scales down to make all data fit.
--
-- @property max_value
-- @tparam number max_value
-- @propemits true false

--- The minimum value.
--
-- @DOC_wibox_widget_graph_min_value_EXAMPLE@
--
-- Note that the min_value is not supported when used along with the stack
-- property.
-- @property min_value
-- @tparam number min_value
-- @propemits true false

--- Set the graph to automatically scale its values. Default is false.
--
--@DOC_wibox_widget_graph_scale1_EXAMPLE@
--
-- @property scale
-- @tparam boolean scale
-- @propemits true false

--- Set the width or the individual steps.
--
-- Note that it isn't supported when used along with stacked graphs.
--
--@DOC_wibox_widget_graph_step_EXAMPLE@
--
-- @property step_width
-- @tparam[opt=1] number step_width
-- @propemits true false

--- Set the spacing between the steps.
--
-- Note that it isn't supported when used along with stacked graphs.
--
--@DOC_wibox_widget_graph_step_spacing_EXAMPLE@
--
-- @property step_spacing
-- @tparam[opt=0] number step_spacing
-- @propemits true false

--- The step shape.
--
--@DOC_wibox_widget_graph_step_shape_EXAMPLE@
--
-- @property step_shape
-- @tparam[opt=rectangle] gears.shape|function step_shape
-- @propemits true false
-- @see gears.shape

--- Set the graph to draw stacks. Default is false.
--
--@DOC_wibox_widget_graph_stacked_EXAMPLE@
-- @property stack
-- @tparam boolean stack
-- @propemits true false

--- Set the graph stacking colors. Order matters.
--
-- @property stack_colors
-- @tparam table stack_colors A table with stacking colors.

--- The graph background color.
--
-- @beautiful beautiful.graph_bg
-- @param color

--- The graph foreground color.
--
-- @beautiful beautiful.graph_fg
-- @param color

--- The graph border color.
--
-- @beautiful beautiful.graph_border_color
-- @param color

local properties = { "width", "height", "border_color", "stack",
                     "stack_colors", "color", "background_color",
                     "max_value", "scale", "min_value", "step_shape",
                     "step_spacing", "step_width", "border_width" }

function graph:draw(_, cr, width, height)
    local max_value = self._private.max_value
    local min_value = self._private.min_value or (
        self._private.scale and math.huge or 0)
    local values = self._private.values

    local step_shape = self._private.step_shape
    local step_spacing = self._private.step_spacing or 0
    local step_width = self._private.step_width or 1
    local bw = self._private.border_width or 1

    local draw_with_lines = not step_shape and step_width == 1

    if draw_with_lines then
        -- Set thin line width for drawing graph bars with lines
        cr:set_line_width(1)
    end

    -- Draw the background first
    cr:set_source(color(self._private.background_color or beautiful.graph_bg or "#000000aa"))
    cr:paint()

    -- Account for the border width
    cr:save()

    if self._private.border_color then
        cr:translate(bw, bw)
        width, height = width - 2*bw, height - 2*bw
    end

    -- Preserve the transform centered at the top-left corner of the graph
    local pristine_transform = step_shape and cr:get_matrix()

    -- Draw a stacked graph
    if self._private.stack then

        if self._private.scale then
            local acc = {}
            for _, v in ipairs(values) do
                for idx, sv in ipairs(v) do
                    acc[idx] = (acc[idx] or 0) + sv
                end
            end
            for _, av in ipairs(acc) do
                if av > max_value then
                    max_value = av
                end
                if min_value > av then
                    min_value = av
                end
            end
        end

        for i = 0, width do
            local rel_i = 0
            local rel_x = i + 0.5

            if self._private.stack_colors then
                for idx, col in ipairs(self._private.stack_colors) do
                    local stack_values = values[idx]
                    if stack_values and i < #stack_values then
                        local value = stack_values[i + 1] + rel_i
                        cr:move_to(rel_x, height * (1 - (rel_i / max_value)))
                        cr:line_to(rel_x, height * (1 - (value / max_value)))
                        cr:set_source(color(col or beautiful.graph_fg or "#ff0000"))
                        cr:stroke()
                        rel_i = value
                    end
                end
            end
        end
    else
        -- Non-stacked graph draws the default value group #1
        values = values[1] or {}

        if self._private.scale then
            for _, v in ipairs(values) do
                if v > max_value then
                    max_value = v
                end
                if min_value > v then
                    min_value = v
                end
            end
            if min_value == max_value then
                -- If all values are equal in an autoscaled graph,
                -- simply draw them in the middle
                min_value, max_value = min_value - 1, max_value + 1
            end
        end

        -- Draw the background on no value
        if #values ~= 0 then
            local baseline_y = height

            for i = 0, #values - 1 do
                local value = values[i + 1]
                if value >= 0 then
                    -- Scale the value so that [min_value..max_value] maps to [0..1]
                    value = (value - min_value) / (max_value - min_value)

                    -- The coordinate of the i-th bar's left edge
                    local x = i*(step_width + step_spacing)

                    -- Drawing bars up from the lower edge of the widget
                    local value_y = height * (1 - value)

                    if step_shape then
                        -- Shift to the bar beginning
                        cr:translate(x, value_y)
                        step_shape(cr, step_width, baseline_y - value_y)
                        -- Undo the shift
                        cr:set_matrix(pristine_transform)
                    else
                        if draw_with_lines then
                            cr:move_to(x + 0.5, value_y)
                            cr:line_to(x + 0.5, baseline_y)
                        else
                            cr:rectangle(x, value_y, step_width, baseline_y - value_y)
                        end
                    end
                end
            end
            cr:set_source(color(self._private.color or beautiful.graph_fg or "#ff0000"))

            if draw_with_lines then
                cr:stroke()
            else
                cr:fill()
            end
        end

    end

    -- Undo the cr:translate() for the border and step shapes
    cr:restore()

    -- Draw the border last so that it overlaps already drawn values
    if self._private.border_color then
        -- We decremented these by two above
        width, height = width + 2*bw, height + 2*bw

        cr:set_line_width(bw)

        -- Draw the border
        cr:rectangle(bw/2, bw/2, width - bw, height - bw)
        cr:set_source(color(self._private.border_color or beautiful.graph_border_color or "#ffffff"))
        cr:stroke()
    end
end

function graph.fit(_graph)
    return _graph._private.width, _graph._private.height
end

--- Add a value to the graph
--
-- @method add_value
-- @tparam number value The value to be added to the graph
-- @tparam[opt] number group The stack color group index.
function graph:add_value(value, group)
    value = value or 0
    group = group or 1

    local max_value = self._private.max_value
    value = math.max(0, value)
    if not self._private.scale then
        value = math.min(max_value, value)
    end

    local values = self._private.values
    if not values[group] then
        values[group] = {}
    end
    values = values[group]

    table.insert(values, 1, value)

    local border_width = 0
    if self._private.border_color then border_width = self._private.border_width*2 end

    -- Ensure we never have more data than we can draw
    while #values > self._private.width - border_width do
        table.remove(values)
    end

    self:emit_signal("widget::redraw_needed")
    return self
end

--- Clear the graph.
--
-- @method clear
function graph:clear()
    self._private.values = {}
    self:emit_signal("widget::redraw_needed")
    return self
end

--- Set the graph height.
--
-- @property height
-- @tparam number height The height to set.
-- @propemits true false

function graph:set_height(height)
    if height >= 5 then
        self._private.height = height
        self:emit_signal("widget::layout_changed")
        self:emit_signal("property::height", height)
    end
    return self
end

--- Set the graph width.
--
-- @property width
-- @param number width The width to set.
-- @propemits true false

function graph:set_width(width)
    if width >= 5 then
        self._private.width = width
        self:emit_signal("widget::layout_changed")
        self:emit_signal("property::width", width)
    end
    return self
end

-- Build properties function
for _, prop in ipairs(properties) do
    if not graph["set_" .. prop] then
        graph["set_" .. prop] = function(_graph, value)
            if _graph._private[prop] ~= value then
                _graph._private[prop] = value
                _graph:emit_signal("widget::redraw_needed")
                _graph:emit_signal("property::"..prop, value)
            end
            return _graph
        end
    end
    if not graph["get_" .. prop] then
        graph["get_" .. prop] = function(_graph)
            return _graph._private[prop]
        end
    end
end

--- Create a graph widget.
--
-- @tparam table args Standard widget() arguments. You should add width and height
-- key to set graph geometry.
-- @treturn wibox.widget.graph A new graph widget.
-- @constructorfct wibox.widget.graph
function graph.new(args)
    args = args or {}

    local width = args.width or 100
    local height = args.height or 20

    if width < 5 or height < 5 then return end

    local _graph = base.make_widget(nil, nil, {enable_properties = true})

    _graph._private.width     = width
    _graph._private.height    = height
    _graph._private.values    = {}
    _graph._private.max_value = 1

    -- Set methods
    _graph.add_value = graph["add_value"]
    _graph.clear = graph["clear"]
    _graph.draw = graph.draw
    _graph.fit = graph.fit

    for _, prop in ipairs(properties) do
        _graph["set_" .. prop] = graph["set_" .. prop]
        _graph["get_" .. prop] = graph["get_" .. prop]
    end

    return _graph
end

function graph.mt:__call(...)
    return graph.new(...)
end

return setmetatable(graph, graph.mt)

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
