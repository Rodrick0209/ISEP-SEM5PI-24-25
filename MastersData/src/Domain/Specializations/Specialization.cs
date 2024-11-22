using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.Specializations
{
  public class Specialization : Entity<SpecializationId>, IAggregateRoot
  {
    
    public string Name { get; }


    private Specialization()
    {
    }



    public Specialization(string name)
    {
      this.Id = new SpecializationId(Guid.NewGuid());
      Name = name;
    }

    public Specialization(SpecializationId id, string name)
    {
      this.Id = id;
      Name = name;
    }

    
 
  }




}