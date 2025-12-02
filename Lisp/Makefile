STACK             := stack
STACK_WORK_DIR    := .stack-work
NPROCS            := $$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
STACK_BUILD_FLAGS := --jobs $(NPROCS)
STACK_NIX_FLAGS   := --nix --system-ghc

STACK_BINARY_NAME := glados-exe
NAME              := glados

all:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) build $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS); \
	else \
		$(STACK) build $(STACK_BUILD_FLAGS); \
	fi
	@$(MAKE) symlink

tests:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) test $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS); \
	else \
		$(STACK) test $(STACK_BUILD_FLAGS); \
	fi

coverage:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) test $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS) --coverage; \
	else \
		$(STACK) test $(STACK_BUILD_FLAGS) --coverage; \
	fi

clean:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) clean $(STACK_NIX_FLAGS); \
	else \
		$(STACK) clean; \
	fi

fclean:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) clean $(STACK_NIX_FLAGS); \
	else \
		$(STACK) clean; \
	fi
	rm -rf $(STACK_WORK_DIR) $(NAME)

re: fclean all

symlink:
    @BIN=$$(stack exec -- which glados-exe 2>/dev/null || true); \
    if [ -z "$$BIN" ]; then \
      echo "glados executable not found, skipping symlink"; \
    else \
      ln -sf "$$BIN" "$(CURDIR)/glados"; \
      echo "Created symlink: $(CURDIR)/glados -> $$BIN"; \
    fi

.PHONY: all clean fclean re test coverage symlink
