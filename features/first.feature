Feature: A first feature
   This is not very useful, we are just testing that we can execute basic
   calculator tests.

   Scenario: Simple additions
      When I enter "1"
      And I enter "2"
      And I enter "+"
      Then I should read 3
