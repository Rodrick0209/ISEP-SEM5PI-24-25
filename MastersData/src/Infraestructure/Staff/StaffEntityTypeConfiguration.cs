using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Infrastructure.StaffMembers
{

    internal class StaffEntityTypeConfiguration : IEntityTypeConfiguration<Staff>
    {
        public void Configure(EntityTypeBuilder<Staff> builder)
        {
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.FullName);
            builder.OwnsOne(b => b.LicenseNumber, ln =>
            {
                ln.Property(l => l.licenseNumber).IsRequired();               
                ln.HasIndex(l => l.licenseNumber).IsUnique();                
            });
            builder.OwnsOne(b => b.Email).HasIndex(e => e.email).IsUnique();
            builder.OwnsOne(b => b.PhoneNumber).HasIndex(p => p.phoneNumber).IsUnique();
        }
    }






}