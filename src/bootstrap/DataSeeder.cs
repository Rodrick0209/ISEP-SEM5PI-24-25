using System;
using System.Linq;
using DDDSample1.Domain.Families;
using DDDSample1.Domain.OperationType;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.User;
using DDDSample1.Infrastructure;
using Microsoft.Extensions.DependencyInjection;

public static class DataSeeder
{
  public static void Seed(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();


    // Example: Add Families if none exist
    if (!context.Families.Any())
    {
      context.Families.AddRange(
          new Family("1", "Family1"),
          new Family("2", "Family2")
      );
    }

    SeedUsers(context,new User("admin@teste.com","admin"),"password");
    
    
    
    SeedOperationType(context,new OperationType("1","New Operation Type",true,new Phase("1"),new Phase("2"),new Phase("3"),new Specialization("Ortopedia")));
    
    context.SaveChanges();
  }


  private static void SeedUsers(DDDSample1DbContext context, User user, string pass)
  {
    PasswordHasher passwordHasher = new PasswordHasher();
    string password = passwordHasher.HashPassword(pass);
    user.SetPassword(password);
    if (!context.Users.Any())
    {
      context.Users.AddRange(user);
    }
  }

  private static void SeedOperationType(DDDSample1DbContext context, OperationType operationType)
  {
    if (!context.OperationTypes.Any())
    {
      context.OperationTypes.Add(operationType);
    }
  }
}
