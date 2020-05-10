#include <stddef.h>
#include <stdbool.h>
#include <dlfcn.h>
#include <pango/pango.h>

typedef typeof(pango_context_new) pango_context_new_t;

static pango_context_new_t *orig_pango_context_new = NULL;
PangoContext *pango_context_new (void) {
	if (!orig_pango_context_new) {
		orig_pango_context_new = dlsym(RTLD_NEXT, "pango_context_new");

		if (!orig_pango_context_new)
			abort();
	}

	PangoContext *ctx = orig_pango_context_new();
	pango_context_set_round_glyph_positions(ctx, false);

	return ctx;
}

typedef typeof(pango_font_map_create_context) pango_font_map_create_context_t;

static pango_font_map_create_context_t *orig_pango_font_map_create_context = NULL;
PangoContext *pango_font_map_create_context (PangoFontMap *fontmap) {
	if (!orig_pango_font_map_create_context) {
		orig_pango_font_map_create_context = dlsym(RTLD_NEXT, "pango_font_map_create_context");

		if (!orig_pango_font_map_create_context)
			abort();
	}

	PangoContext *ctx = orig_pango_font_map_create_context(fontmap);
	pango_context_set_round_glyph_positions(ctx, false);

	return ctx;
}
