"""Guard the permissions that allow a native object to be suspended."""
import unittest
from audit_native_leases import dialogue_only, plain_icon_text


class DialogueGraphTests(unittest.TestCase):
    commands = ["end", "jumptextfaceplayer", "checkevent", "iftrue", "setevent",
                "iftruefwd", "faceplayer", "iftrue_jumptextfaceplayer", "giveitem"]

    def accepts(self, body, offset=0, remaining=32):
        data = bytearray(0x4000)
        data[offset:offset + len(body)] = body
        return dialogue_only(data, self.commands, offset, 0, remaining)

    def test_flag_read_and_both_pure_branches(self):
        self.assertTrue(self.accepts(bytes([2, 1, 0, 5, 3, 1, 0, 0, 1, 0, 0])))

    def test_any_state_writing_branch_protects_actor(self):
        self.assertFalse(self.accepts(bytes([2, 1, 0, 5, 3, 1, 0, 0, 4, 1, 0, 0])))

    def test_conditional_text_still_checks_fallthrough(self):
        self.assertFalse(self.accepts(bytes([7, 0, 0, 8, 1, 0, 0])))
        self.assertTrue(self.accepts(bytes([7, 0, 0, 1, 0, 0])))

    def test_bank_escape_and_unknown_command_protect_actor(self):
        self.assertFalse(self.accepts(bytes([3, 0, 0x80, 0])))
        self.assertFalse(self.accepts(bytes([255])))

    def test_cycles_and_budget_exhaustion_protect_actor(self):
        self.assertFalse(self.accepts(bytes([3, 0, 0x40, 0])))
        self.assertFalse(self.accepts(bytes([6, 6, 6, 0]), remaining=3))

    def test_truncated_terminal_protects_actor(self):
        self.assertFalse(self.accepts(bytes([1]), offset=0x3fff))
        self.assertTrue(self.accepts(bytes([1, 0, 0]), offset=0x3ffd))

    def test_icon_text_requires_literal_terminated_local_dialogue(self):
        self.assertTrue(plain_icon_text('CowText:\n\ttext "Moo!"\n\tline "Hello."\n\tdone\n', ['CowText']))
        self.assertFalse(plain_icon_text('CowText:\n\ttext "Moo!"\n', ['CowText']))
        self.assertFalse(plain_icon_text('OtherText:\n\ttext "Moo!"\n\tdone\n', ['CowText']))

    def test_executable_or_indirect_icon_text_is_protected(self):
        for unsafe in ('text_asm GiveItem', 'text_far OtherText', 'text_jump OtherText'):
            self.assertFalse(plain_icon_text(f'CowText:\n\ttext "Moo!"\n\t{unsafe}\n\tdone\n', ['CowText']))


if __name__ == "__main__":
    unittest.main()
