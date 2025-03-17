
#ifdef __KOS_KERNEL__
  extern int printk(const char*, ...);
  #define PSL_PRINT_FN(fmt, ...) printk(fmt, ##__VA_ARGS__)
#else
  #include <stdio.h>
  #define PSL_PRINT_FN(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#endif
    
#include "kss/audit.h"
#include "kss/kss.h"
#include "kss/security-gen-api.h"
#include "kss/services.h"
#include "kss/types.h"
#include "kss/utf8.h"
#include "kssaudit/constants.h"
#include "kssaudit/encode.h"
#include "nk/stand.h"
#include "nk/types.h"
#include <qcbor/qcbor_encode.h>
typedef enum kss_audit_profile {
            KSS_AUDIT_NEVER = 0,
            KSS_AUDIT_DENIED = 1,
            KSS_AUDIT_GRANTED = 2,
            KSS_AUDIT_ALWAYS = 3,
        } kss_audit_profile_t;
#include <nk/transport.h>
#include <nk/types.h>
#include <nk/layout.h>

#define PSL_ENABLE_TRACE
#ifdef PSL_ENABLE_TRACE
  #define CLS_PSL            "\033[0;37m"
  #define CLS_ERROR          "\033[0;31m"
  #define CLS_CRIT           "\033[1;31m"

  #define CLS_KSS            "\033[0;35m"
  #define CLS_KSS_GRANTED    "\033[0;34m"
  #define CLS_KSS_DENIED     "\033[0;33m"

  #define CLS_AUDIT          "\033[0;90m"
  #define CLS_AUDIT_MSG      "\033[1;37m"

  #define CLS_PROVIDER       "\033[0;36m"
  #define CLS_PROVIDER_ERROR "\033[0;31m"

  #define CLS_TEST           "\033[0;37m"
  #define CLS_TEST_ERROR     "\033[0;31m"

  #define                         \
    PSL_TRACE(cls, sys, fmt, ...) \
      PSL_PRINT_FN(               \
        cls                       \
        sys                       \
        "\t[ %s | %d ]\t"         \
        fmt                       \
        "\033[0m\n",              \
        __func__, __LINE__,       \
        ##__VA_ARGS__             \
      )
#else
  #define PSL_TRACE(cls, fmt, ...) \
    ((void)0)
#endif


nk_err_t __kss_init(void)
{
    kss_slot_t currentSlotId = 0;

    nk_unused(currentSlotId);
    PSL_TRACE(CLS_KSS, "KSS", "Init");
    PSL_TRACE(CLS_KSS, "KSS", "Init done");
    return NK_EOK;
}

void __kss_fini(void)
{
    PSL_TRACE(CLS_KSS, "KSS", "Fini");
    PSL_TRACE(CLS_KSS, "KSS", "Fini done");
}
kss_eid_t __kss_get_eid(const char *eiid)
{

    PSL_TRACE(CLS_KSS, "KSS", "_kss_get_eid");
    /*
    const char **p;

    PSL_TRACE(CLS_KSS, "KSS", "eiid: %s", eiid);
    nk_assert(eiid != NK_NULL);
    p = (const char **) nk_bsearch(&eiid, eiids, nk_array_size(eiids),
                                   sizeof(eiids[0]), get_eid_cmp);
    if (p != NK_NULL) {
        PSL_TRACE(CLS_KSS, "KSS", "eiid: %s -> eid: %ld", eiid, p - eiids);
        return (kss_eid_t) (p - eiids);
    } else {
        PSL_TRACE(CLS_ERROR, "KSS", "eiid: %s -> not found", eiid);
        return KSS_INVALID_EID;
    }
     */
    //return KSS_INVALID_EID;
    return 0;
}
nk_size_t __kss_get_num_slots(void)
{
    PSL_TRACE(CLS_KSS, "KSS", "slots: %d", 0);
    return 0;
}
nk_err_t __kss_unit_test(void)
{
    PSL_TRACE(CLS_TEST, "Test", "Unit tests were not generated.");
    return NK_EOK;
}

nk_err_t __kss_execute(kss_decision_t *decision, kss_eid_t src_eid,
                       nk_sid_t src_sid, kss_eid_t dst_eid, nk_sid_t dst_sid,
                       const struct nk_message *message, const
                       struct nk_arena *arena,
                       struct kss_audit_message *audit_message)
{
    nk_unused(src_eid);
    nk_unused(src_sid);
    nk_unused(dst_eid);
    nk_unused(dst_sid);
    nk_unused(message);
    nk_unused(arena);
    nk_unused(audit_message);

    *decision = KSS_GRANT;
    PSL_TRACE(CLS_PSL, "PSL", "src: %d, dst: %d", src_eid, dst_eid);
    return NK_EOK;
}
nk_err_t __kss_request(kss_decision_t *decision, kss_eid_t src_eid,
                       nk_sid_t src_sid, kss_eid_t dst_eid, nk_sid_t dst_sid,
                       const struct nk_message *message, const
                       struct nk_arena *arena,
                       struct kss_audit_message *audit_message)
{
    nk_unused(src_eid);
    nk_unused(src_sid);
    nk_unused(dst_eid);
    nk_unused(dst_sid);
    nk_unused(message);
    nk_unused(arena);
    nk_unused(audit_message);

    *decision = KSS_GRANT;
    return NK_EOK;
}

nk_err_t __kss_response(kss_decision_t *decision, kss_eid_t src_eid,
                        nk_sid_t src_sid, kss_eid_t dst_eid, nk_sid_t dst_sid,
                        const struct nk_message *message, const
                        struct nk_arena *arena,
                        struct kss_audit_message *audit_message)
{

    nk_unused(src_eid);
    nk_unused(src_sid);
    nk_unused(dst_eid);
    nk_unused(dst_sid);
    nk_unused(message);
    nk_unused(arena);
    nk_unused(audit_message);

    *decision = KSS_GRANT;
    return NK_EOK;
}
nk_err_t __kss_error(kss_decision_t *decision, kss_eid_t src_eid,
                     nk_sid_t src_sid, kss_eid_t dst_eid, nk_sid_t dst_sid,
                     const struct nk_message *message, const
                     struct nk_arena *arena,
                     struct kss_audit_message *audit_message)
{

    nk_unused(src_eid);
    nk_unused(src_sid);
    nk_unused(dst_eid);
    nk_unused(dst_sid);
    nk_unused(message);
    nk_unused(arena);
    nk_unused(audit_message);

    *decision = KSS_GRANT;
    return NK_EOK;
}
nk_err_t __kss_security(kss_decision_t *decision, kss_eid_t driver_eid,
                        nk_sid_t driver_sid, const struct nk_message *message,
                        const struct nk_arena *arena,
                        struct kss_audit_message *audit_message)
{

    nk_unused(driver_eid);
    nk_unused(driver_sid);
    nk_unused(message);
    nk_unused(arena);
    nk_unused(audit_message);
    *decision = KSS_GRANT;
    return NK_EOK;
}
