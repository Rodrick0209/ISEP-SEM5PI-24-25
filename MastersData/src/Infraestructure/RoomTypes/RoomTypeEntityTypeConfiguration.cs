using DDDSample1.Domain.RoomTypes;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.RoomTypes
{
    internal class RoomTypeEntityTypeConfiguration : IEntityTypeConfiguration<RoomType>
    {
        public void Configure(EntityTypeBuilder<RoomType> builder)
        {
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.Name).HasIndex(n => n.fullName).IsUnique();
        }
    }
}