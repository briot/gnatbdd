Feature: A first feature
   This is not very useful, we are just testing that we can execute basic
   calculator tests.

   Background:
      Given an empty calculator

   Scenario: Simple additions
      When I enter "1"
      And I enter "2"
      And I enter "+"
      Then I should read 3

   Scenario Outline: testing operators
      When I enter "<first>"
      And I enter "<second>"
      And I enter "<operation>"
      Then I should read <result>
      Examples:
         | first  | second | operation | result |
         |  10    |  20    |   +       | 30     |
         |  20    |  10    |   -       | 10     |
         |  10    |  20    |   +       | 40     |

   Scenario: Checking stack contents
      When I enter "10"
      And  I enter "20"
      And  I enter "30"
      And  I enter "40"
      Then the stack should contain
         | value |
         | 10    |
         | 20    |
         | 31    |
         | 40    |
