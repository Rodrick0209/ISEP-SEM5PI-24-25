using System;
using System.Collections.Generic;
using System.Diagnostics;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.Specializations
{
  public class Specialization : Entity<SpecializationId>, IAggregateRoot
  {
    
    public string Name { get;set; }


    private Specialization()
    {
    }



    public Specialization(string name)
    {
      Id = new SpecializationId(Guid.NewGuid());
      Name = name;
    }

    public Specialization(SpecializationId id, string name)
    {
      Id = id;
      Name = name;
    }

    public void changeName(string name)
    {
      Name = name;
    }

 
  }




}