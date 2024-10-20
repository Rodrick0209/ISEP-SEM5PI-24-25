using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.User;

namespace DDDSample1.Infrastructure.Users

{
    internal class UserEntityTypeConfiguration : IEntityTypeConfiguration<User>
    {
        public void Configure ( EntityTypeBuilder<User> builder){
            
         //   builder.("User", SchemaNames.DDDSample1);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.password);
            builder.OwnsOne(b => b.role);
            builder.OwnsOne(b => b.email);
            builder.OwnsOne(b => b.resetPasswordToken);
            builder.OwnsOne(b => b.resetPasswordTokenExpiration);
            builder.OwnsOne(b => b.loginFailCounter);
            builder.OwnsOne(b => b.accountBlockedTime);
            builder.OwnsOne(b => b.accountConfirmed);
            builder.OwnsOne(b => b.confirmationRegisterPatientToken);
            builder.OwnsOne(b => b.confirmationRegisterPatientTokenExpiration);

        }
    }



}