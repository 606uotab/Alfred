defmodule Alfred.AuthTest do
  use ExUnit.Case

  alias Alfred.Auth

  describe "access matrix" do
    test "master can access all vaults" do
      assert Auth.can_access?(:master, "creator")
      assert Auth.can_access?(:master, "users")
      assert Auth.can_access?(:master, "culture")
    end

    test "master can write to all vaults" do
      assert Auth.can_write?(:master, "creator")
      assert Auth.can_write?(:master, "users")
      assert Auth.can_write?(:master, "culture")
    end

    test "admin cannot access creator" do
      refute Auth.can_access?(:admin, "creator")
    end

    test "admin can access users and culture" do
      assert Auth.can_access?(:admin, "users")
      assert Auth.can_access?(:admin, "culture")
    end

    test "admin can write to users and culture" do
      assert Auth.can_write?(:admin, "users")
      assert Auth.can_write?(:admin, "culture")
    end

    test "user cannot access creator or users" do
      refute Auth.can_access?(:user, "creator")
      refute Auth.can_access?(:user, "users")
    end

    test "user can access culture (read only)" do
      assert Auth.can_access?(:user, "culture")
      refute Auth.can_write?(:user, "culture")
    end

    test "unknown vault returns false" do
      refute Auth.can_access?(:master, "nonexistent")
      refute Auth.can_write?(:admin, "nonexistent")
    end
  end

  describe "access_level" do
    test "returns correct levels" do
      assert Auth.access_level(:master, "creator") == :write
      assert Auth.access_level(:admin, "creator") == :none
      assert Auth.access_level(:user, "culture") == :read
      assert Auth.access_level(:user, "users") == :none
    end
  end

  describe "identify_user" do
    test "returns user role with name" do
      assert {:ok, :user, "Jean"} = Auth.identify_user("Jean")
    end
  end
end
