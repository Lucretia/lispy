#include "../mpc/mpc.h"

mpc_err_t *helper_language2 (const int flags,
                             const char *language,
                             const mpc_parser_t *p1,
                             const mpc_parser_t *p2)
{
    return mpca_lang (flags, language, p1, p2);
}

mpc_err_t *helper_language3 (const int flags,
                             const char *language,
                             const mpc_parser_t *p1,
                             const mpc_parser_t *p2,
                             const mpc_parser_t *p3)
{
    return mpca_lang (flags, language, p1, p2, p3);
}

mpc_err_t *helper_language4 (const int flags,
                             const char *language,
                             const mpc_parser_t *p1,
                             const mpc_parser_t *p2,
                             const mpc_parser_t *p3,
                             const mpc_parser_t *p4)
{
    return mpca_lang (flags, language, p1, p2, p3, p4);
}

mpc_err_t *helper_language5 (const int flags,
                             const char *language,
                             const mpc_parser_t *p1,
                             const mpc_parser_t *p2,
                             const mpc_parser_t *p3,
                             const mpc_parser_t *p4,
                             const mpc_parser_t *p5)
{
    return mpca_lang (flags, language, p1, p2, p3, p4, p5);
}
