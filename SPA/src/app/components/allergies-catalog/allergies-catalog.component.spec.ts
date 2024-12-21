import { ComponentFixture, TestBed } from '@angular/core/testing';
import { AllergiesCatalogComponent } from './allergies-catalog.component';
import { ListAllergiesCatalogComponent } from '../list-allergies-catalog/list-allergies-catalog.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AllergiesCatalogComponent', () => {
  let component: AllergiesCatalogComponent;
  let fixture: ComponentFixture<AllergiesCatalogComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListAllergiesCatalogComponent, HttpClientTestingModule]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AllergiesCatalogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});