using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace Domain.Staff
{
  public class Specialization : IValueObject
  {
    public string Name { get; }

    public Specialization(string name)
    {
      Name = name;
    }

    public override bool Equals(object obj)
    {
      if (obj == null || GetType() != obj.GetType())
        return false;

      var other = (Specialization)obj;
      return Name == other.Name;
    }

 
  }
}