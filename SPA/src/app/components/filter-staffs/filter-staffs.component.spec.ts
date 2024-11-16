import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';  // Importa FormsModule para suportar ngModel
import { FilterStaffsComponent } from './filter-staffs.component';

describe('FilterStaffsComponent', () => {
  let component: FilterStaffsComponent;
  let fixture: ComponentFixture<FilterStaffsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FormsModule],  // Certifique-se de que o FormsModule está importado
      declarations: [FilterStaffsComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterStaffsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should emit filter with phone number', () => {
    const filter = { 
      name: 'staffMario', 
      licenseNumber: '12346', 
      phoneNumber: '+951999999998', 
      email: 'emaill@gmail.com',
      specialization: 'Obstetricia'
    };

    spyOn(component.filterChanged, 'emit');

    component.filter = filter;
    component.applyFilter();

    expect(component.filterChanged.emit).toHaveBeenCalledWith(filter);
  });
});
