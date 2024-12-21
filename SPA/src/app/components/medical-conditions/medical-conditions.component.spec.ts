import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MedicalConditionsComponent } from './medical-conditions.component';
import { ListMedicalConditionsComponent } from '../list-medical-conditions/list-medical-conditions.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('MedicalConditionsComponent', () => {
  let component: MedicalConditionsComponent;
  let fixture: ComponentFixture<MedicalConditionsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListMedicalConditionsComponent, HttpClientTestingModule]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(MedicalConditionsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});